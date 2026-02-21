{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import AST
import Parser (parseTerm)
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
            parseTerm "true" @?= Right TmTrue
        , testCase "false" $
            parseTerm "false" @?= Right TmFalse
        , testCase "number" $
            parseTerm "42" @?= Right (TmNumber 42)
        , testCase "addition" $
            parseTerm "1 + 2" @?= Right (TmAdd (TmNumber 1) (TmNumber 2))
        , testCase "addition is left-associative" $
            parseTerm "1 + 2 + 3" @?= Right (TmAdd (TmAdd (TmNumber 1) (TmNumber 2)) (TmNumber 3))
        , testCase "ternary" $
            parseTerm "true ? 1 : 2" @?= Right (TmIf TmTrue (TmNumber 1) (TmNumber 2))
        , testCase "nested ternary" $
            parseTerm "true ? false ? 1 : 2 : 3"
                @?= Right (TmIf TmTrue (TmIf TmFalse (TmNumber 1) (TmNumber 2)) (TmNumber 3))
        , testCase "parens" $
            parseTerm "(true)" @?= Right TmTrue
        , testCase "parens in addition" $
            parseTerm "1 + (2 + 3)" @?= Right (TmAdd (TmNumber 1) (TmAdd (TmNumber 2) (TmNumber 3)))
        , testCase "leading/trailing whitespace" $
            parseTerm "  42  " @?= Right (TmNumber 42)
        , testCase "parse error" $
            case parseTerm "???" of
                Left _ -> pure ()
                Right t -> assertFailure $ "expected error but got: " ++ show t
        ]

-- TypeChecker

typecheckerTests :: TestTree
typecheckerTests =
    testGroup
        "TypeChecker"
        [ testCase "true :: Boolean" $
            typecheck TmTrue @?= Right TyBoolean
        , testCase "false :: Boolean" $
            typecheck TmFalse @?= Right TyBoolean
        , testCase "number :: Number" $
            typecheck (TmNumber 0) @?= Right TyNumber
        , testCase "add :: Number" $
            typecheck (TmAdd (TmNumber 1) (TmNumber 2)) @?= Right TyNumber
        , testCase "ternary with boolean branches :: Boolean" $
            typecheck (TmIf TmTrue TmTrue TmFalse) @?= Right TyBoolean
        , testCase "ternary with number branches :: Number" $
            typecheck (TmIf TmTrue (TmNumber 1) (TmNumber 2)) @?= Right TyNumber
        , testCase "ternary: condition must be Boolean" $
            typecheck (TmIf (TmNumber 1) TmTrue TmFalse)
                @?= Left (Unexpected TyNumber TyBoolean)
        , testCase "ternary: branch types must match" $
            typecheck (TmIf TmTrue TmTrue (TmNumber 1))
                @?= Left (Mismatched TyBoolean TyNumber)
        , testCase "add left must be Number" $
            typecheck (TmAdd TmTrue (TmNumber 1))
                @?= Left (Unexpected TyBoolean TyNumber)
        , testCase "add right must be Number" $
            typecheck (TmAdd (TmNumber 1) TmFalse)
                @?= Left (Unexpected TyBoolean TyNumber)
        ]
