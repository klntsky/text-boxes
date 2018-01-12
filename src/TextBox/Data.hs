{-|
Module      : TextBox.Data
Description : TextBox definition.
Copyright   : (c) klntsky, 2018
License     : PublicDomain
Maintainer  : klntsky@gmail.com
Stability   : experimental

'TextBox' definition.
-}
module TextBox.Data
  (
    TextBox (..)
  , getWidth
  , getHeight
  )
where

import TextBox.StringLike
import TextBox.StringLike.String


{- | Data type representing rectangle-shaped block of text.

It is defined as:

@
data TextBox a = TextBox Int Int [a]
@

where the last argument of the constructor is a list of lines. -}
data TextBox a = TextBox Int Int [a]
  deriving Eq

instance Show a => Show (TextBox a) where
  show (TextBox w h ls) = "TextBox (" ++ show w ++ "x" ++ show h ++ ")\n" ++ show ls ++ "\n"



getWidth, getHeight :: StringLike a => TextBox a -> Int
getWidth (TextBox w _ _) = w
getHeight (TextBox _ h _) = h
