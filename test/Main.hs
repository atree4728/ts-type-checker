{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AST
import Parser (parse)
import Test.Tasty
import Test.Tasty.HUnit
import TypeChecker

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ts-type-checker" [parserTests, typecheckerTests]

-- Parser

parserTests :: TestTree
parserTests =
  testGroup
    "Parser"
    [ testCase "true" $
        parse "true" @?= Right TmTrue,
      testCase "false" $
        parse "false" @?= Right TmFalse,
      testCase "number" $
        parse "42" @?= Right (TmNumber 42),
      testCase "addition" $
        parse "1 + 2" @?= Right (TmAdd (TmNumber 1) (TmNumber 2)),
      testCase "addition is left-associative" $
        parse "1 + 2 + 3" @?= Right (TmAdd (TmAdd (TmNumber 1) (TmNumber 2)) (TmNumber 3)),
      testCase "ternary" $
        parse "true ? 1 : 2" @?= Right (TmIf TmTrue (TmNumber 1) (TmNumber 2)),
      testCase "nested ternary" $
        parse "true ? false ? 1 : 2 : 3"
          @?= Right (TmIf TmTrue (TmIf TmFalse (TmNumber 1) (TmNumber 2)) (TmNumber 3)),
      testCase "parens" $
        parse "(true)" @?= Right TmTrue,
      testCase "parens in addition" $
        parse "1 + (2 + 3)" @?= Right (TmAdd (TmNumber 1) (TmAdd (TmNumber 2) (TmNumber 3))),
      testCase "leading/trailing whitespace" $
        parse "  42 \n + 1 " @?= Right (TmAdd (TmNumber 42) (TmNumber 1)),
      testCase "parse error" $
        case parse "???" of
          Left _ -> pure ()
          Right t -> assertFailure $ "expected error but got: " ++ show t,
      testCase "variable" $
        parse "x" @?= Right (TmVar "x"),
      testCase "keyword" $
        case parse "number" of
          Left _ -> pure ()
          Right t -> assertFailure $ "expected error but got: " ++ show t,
      testCase "lambda" $
        parse "(n: number, b: boolean) => 1"
          @?= Right (TmArrow [Param "n" TyNumber, Param "b" TyBoolean] (TmNumber 1)),
      testCase "call by lambda" $
        parse "((n: number) => 1)(0)"
          @?= Right (TmApp (TmArrow [Param "n" TyNumber] (TmNumber 1)) [TmNumber 0]),
      testCase "call by unit" $
        parse "f()"
          @?= Right (TmApp {func = TmVar "f", args = []}),
      testCase "seq" $
        parse "1;2;;" @?= Right (TmSeq {body = TmNumber {n = 1}, rest = TmNumber {n = 2}}),
      testCase "const" $
        parse "const f = () => 0;" @?= Right (TmConst "f" (TmArrow [] (TmNumber 0)) (TmVar "f")),
      testCase "empty object" $
        parse "{}" @?= Right (TmObjNew []),
      testCase "object with props" $
        parse "{foo: 1, bar: true}"
          @?= Right (TmObjNew [PropTerm "foo" (TmNumber 1), PropTerm "bar" TmTrue]),
      testCase "object get" $
        parse "{}.foo" @?= Right (TmObjGet (TmObjNew []) "foo"),
      testCase "object get chain" $
        parse "{}.foo.bar" @?= Right (TmObjGet (TmObjGet (TmObjNew []) "foo") "bar"),
      testCase "object get then call" $
        parse "{}.foo()" @?= Right (TmApp (TmObjGet (TmObjNew []) "foo") [])
    ]

-- TypeChecker

typecheckerTests :: TestTree
typecheckerTests =
  testGroup
    "TypeChecker"
    [ testCase "true :: Boolean" $
        typecheck TmTrue @?= Right TyBoolean,
      testCase "false :: Boolean" $
        typecheck TmFalse @?= Right TyBoolean,
      testCase "number :: Number" $
        typecheck (TmNumber 0) @?= Right TyNumber,
      testCase "add :: Number" $
        typecheck (TmAdd (TmNumber 1) (TmNumber 2)) @?= Right TyNumber,
      testCase "ternary with boolean branches :: Boolean" $
        typecheck (TmIf TmTrue TmTrue TmFalse) @?= Right TyBoolean,
      testCase "ternary with number branches :: Number" $
        typecheck (TmIf TmTrue (TmNumber 1) (TmNumber 2)) @?= Right TyNumber,
      testCase "ternary: condition must be Boolean" $
        typecheck (TmIf (TmNumber 1) TmTrue TmFalse)
          @?= Left (Unexpected TyNumber TyBoolean),
      testCase "ternary: branch types must match" $
        typecheck (TmIf TmTrue TmTrue (TmNumber 1))
          @?= Left (Mismatched TyBoolean TyNumber),
      testCase "add left must be Number" $
        typecheck (TmAdd TmTrue (TmNumber 1))
          @?= Left (Unexpected TyBoolean TyNumber),
      testCase "add right must be Number" $
        typecheck (TmAdd (TmNumber 1) TmFalse)
          @?= Left (Unexpected TyBoolean TyNumber),
      testCase "unbounded variable" $ typecheck (TmVar "x") @?= Left (Unbounded "x"),
      testCase "() => 0 :: () => Number" $
        typecheck (TmArrow [] (TmNumber 0)) @?= Right (TyArrow [] TyNumber),
      testCase "(n: number) => 0 :: (number) => Number" $
        typecheck (TmArrow [Param "n" TyNumber] (TmNumber 0))
          @?= Right (TyArrow [Param "n" TyNumber] TyNumber),
      testCase "(b: boolean) => true :: (boolean) => Boolean" $
        typecheck (TmArrow [Param "b" TyBoolean] TmTrue)
          @?= Right (TyArrow [Param "b" TyBoolean] TyBoolean),
      testCase "apply function" $
        typecheck (TmApp (TmArrow [Param "n" TyNumber] (TmNumber 0)) [TmNumber 1])
          @?= Right TyNumber,
      testCase "apply function returning boolean" $
        typecheck (TmApp (TmArrow [Param "n" TyNumber] TmTrue) [TmNumber 1])
          @?= Right TyBoolean,
      testCase "apply non-function" $
        typecheck (TmApp (TmNumber 0) [])
          @?= Left (NotAFunction TyNumber),
      testCase "apply with wrong arg type" $
        typecheck (TmApp (TmArrow [Param "n" TyNumber] (TmNumber 0)) [TmTrue])
          @?= Left (UnexpectedApp [TyBoolean] [TyNumber]),
      testCase "apply with wrong arity" $
        typecheck (TmApp (TmArrow [] (TmNumber 0)) [TmNumber 1])
          @?= Left (UnexpectedApp [TyNumber] []),
      testCase "apply multi-arg function" $
        typecheck (TmApp (TmArrow [Param "n" TyNumber, Param "b" TyBoolean] (TmNumber 0)) [TmNumber 1, TmTrue])
          @?= Right TyNumber,
      testCase "apply multi-arg with wrong arg types" $
        typecheck (TmApp (TmArrow [Param "n" TyNumber, Param "b" TyBoolean] (TmNumber 0)) [TmTrue, TmNumber 1])
          @?= Left (UnexpectedApp [TyBoolean, TyNumber] [TyNumber, TyBoolean]),
      testCase "seq :: type of rest" $
        typecheck (TmSeq TmTrue (TmNumber 1))
          @?= Right TyNumber,
      testCase "seq: body type error propagates" $
        typecheck (TmSeq (TmAdd TmTrue (TmNumber 1)) (TmNumber 0))
          @?= Left (Unexpected TyBoolean TyNumber),
      testCase "const binds variable" $
        typecheck (TmConst "x" (TmNumber 0) (TmVar "x"))
          @?= Right TyNumber,
      testCase "const: bound variable used in app" $
        typecheck (TmConst "f" (TmArrow [] TmTrue) (TmApp (TmVar "f") []))
          @?= Right TyBoolean,
      testCase "const: body type error propagates" $
        typecheck (TmConst "x" (TmAdd TmTrue (TmNumber 1)) (TmVar "x"))
          @?= Left (Unexpected TyBoolean TyNumber),
      testCase "const: rest refers to bound type" $
        typecheck (TmConst "n" (TmNumber 42) (TmAdd (TmVar "n") (TmNumber 1)))
          @?= Right TyNumber
    ]
