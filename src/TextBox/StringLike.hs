{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : TextBox.StringLike
Description : Generalization of multi-line string that can be splitted into a
 list of lines.
Copyright   : (c) klntsky, 2018
License     : PublicDomain
Maintainer  : klntsky@gmail.com
Stability   : experimental

Contains 'StringLike' typeclass. -}
module TextBox.StringLike
  (
    StringLike (..)
  , mkSpaces
  , mkRect
  )
where

import Data.List (intersperse)


{- | Typeclass that generalizes the notion of a multi-line string that can be
splitted into a list of lines. It also contains functions for padding and
trimming strings.

Rules:

- @width empty == 0@
- @height empty == 1@
- @width space == 1@
- @height space == 1@
- @height newline == 2@
- @map height . splitLines = map (const 1) . splitLines@
- @width . inverse = width@
-}
class StringLike a where
  -- | Empty string.
  empty :: a
  -- | String containing single space character (used for padding).
  space :: a
  -- | Newline character.
  newline :: a
  -- | Get width of a text block (i.e. length of the widest line)
  width :: a -> Int
  width = maximum . (0:) . map lineLength . splitLines
  -- | Get length of a single line (should be more efficient version
  -- of 'width', it is known that @a@ will never contain 'newline's).
  lineLength :: a -- ^ @a@
    -> Int
  lineLength = width
  -- | Get height of a text block (i.e. count of lines)
  height :: a -> Int
  height str =
    let len = length (splitLines str) in
    if len == 0 then 1 else len
  -- | Convert a text block to a list of lines.
  splitLines :: a -> [a]
  -- | Join lines to form a text block.
  joinLines :: [a] -> a
  joinLines = foldr append empty . intersperse newline
  -- | Used to infer 'Show' instance.
  toString :: a -> String
  toString = const "TextBox"
  -- | Concatenate strings.
  append :: a -> a -> a
  -- | Take @n@ characters from the left. Returns the entire string if the length
  -- is less than or equal to @n@.
  takeN :: Int -- ^ n
    -> a -> a
  -- | Drop @n@ characters from the left. Must return 'empty' if the length is less than or equal to @n@.
  dropN :: Int -> a -> a
  -- | Reverse text line.
  inverse :: a -> a


mkSpaces :: StringLike a => Int -> a
mkSpaces n = foldr append empty (replicate n space)


mkRect :: StringLike a => Int -> Int -> [a]
mkRect w h = replicate h (mkSpaces w)
