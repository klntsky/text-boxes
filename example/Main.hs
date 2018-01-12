module Main where

import TextBox.Data
import TextBox.Utils
import TextBox.Internals
import TextBox.Operators
import TextBox.StringLike

import System.Environment (getArgs)
import Control.Monad (when)


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


main = do
  args <- getArgs
  when ("--print-examples" `elem` args) . mapM_ putStrLn $
    map (\(s, f) -> "@\n" ++ s ++ " a b = \n" ++ fromTextBox (f a b) ++ "\n@\n")
    [ ("hJoinWith topPadder",          hJoinWith topPadder)
    , ("hJoinWith bottomPadder",       hJoinWith bottomPadder)
    , ("hJoinWith centerHeightPadder", hJoinWith centerHeightPadder)
    , ("vJoinWith rightPadder",        vJoinWith rightPadder)
    , ("vJoinWith leftPadder",         vJoinWith leftPadder)
    , ("vJoinWith centerWidthPadder",  vJoinWith centerWidthPadder)
    ]
