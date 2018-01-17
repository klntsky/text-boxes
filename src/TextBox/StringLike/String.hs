{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : TextBox.StringLike.String
Description : instance StringLike String where ...
Copyright   : (c) klntsky, 2018
License     : PublicDomain
Maintainer  : klntsky@gmail.com
Stability   : experimental
-}
module TextBox.StringLike.String () where

import TextBox.StringLike

import Data.List.Split (splitOn)
import Data.List (intersperse)


instance StringLike String where
  empty = ""
  space = " "
  newline = "\n"
  lineLength = length
  height str
    | str == empty = 1
    | otherwise = length (splitLines str)
  splitLines = splitOn "\n"
  joinLines = concat . intersperse "\n"
  toString = id
  append = (++)
  takeN = take
  dropN = drop
  inverse = reverse
