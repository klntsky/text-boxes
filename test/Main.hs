module Main where

import Test.Hspec

import TextBox.Data
import TextBox.Utils
import TextBox.Internals
import TextBox.Operators
import TextBox.StringLike
import Data.Ratio


import qualified Test.TextBox.StringLike.String as TSS
import qualified Test.TextBox.Utils as U

main = do
  printCombinations
  hspec $ do
    TSS.spec
    U.spec



a = toTextBox $
    "+---+\n" ++
    "| A |\n" ++
    "+---+"
b = toTextBox $
    "+-----+\n" ++
    "|     |\n" ++
    "|  B  |\n" ++
    "|     |\n" ++
    "+-----+"

printCombinations =
  mapM_ putStrLn $
  map (\(s, f) -> "@\n" ++ s ++ " a b = \n" ++ fromTextBox (f a b) ++ "\n@\n")
  [
    ("hJoinWith topPadder",          hJoinWith topPadder)
  , ("hJoinWith bottomPadder",       hJoinWith bottomPadder)
  , ("hJoinWith centerHeightPadder", hJoinWith centerHeightPadder)
  , ("vJoinWith rightPadder",        vJoinWith rightPadder)
  , ("vJoinWith leftPadder",         vJoinWith leftPadder)
  , ("vJoinWith centerWidthPadder",  vJoinWith centerWidthPadder)
  ]
