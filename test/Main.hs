module Main where

import Test.Hspec

import TextBox.Data
import TextBox.Utils
import TextBox.Internals
import TextBox.Operators
import TextBox.StringLike

import qualified Test.TextBox.StringLike.String as TSS
import qualified Test.TextBox.Utils.Transform as TUT
import qualified Test.TextBox.Utils as U


main = do
  hspec $ do
    TSS.spec
    U.spec
    TUT.spec
