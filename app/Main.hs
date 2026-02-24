module Main where

import Data.Text.IO qualified as TIO
import Lang (langcheck)

main :: IO ()
main = TIO.getContents >>= (print . langcheck)