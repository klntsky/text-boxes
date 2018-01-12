{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : TextBox.StringLike.Text
Description : instance StringLike Text where ...
Copyright   : (c) klntsky, 2018
License     : PublicDomain
Maintainer  : klntsky@gmail.com
Stability   : experimental
-}
module TextBox.StringLike.Text () where

import TextBox.StringLike

import Data.List.Split (splitOn)
import Data.List (intersperse)
import qualified Data.Text as T


instance StringLike T.Text where
  empty = T.empty
  space = T.singleton ' '
  newline = T.singleton '\n'
  width = maximum . (0:) . map lineLength . splitLines
  lineLength = T.length
  height str
    | str == empty = 1
    | otherwise = length (splitLines str)
  splitLines = T.split (=='\n')
  joinLines = foldr (\t -> T.append t . T.cons '\n') T.empty
  toString = show
  append = T.append
  takeN = T.take
  dropN = T.drop
  inverse = T.reverse
