{-|
Module      : TextBox
Description : Main module of the library.
Copyright   : (c) klntsky, 2018
License     : PublicDomain
Maintainer  : klntsky@gmail.com
Stability   : experimental

This module contains tools for working with rectangle-shaped blocks of
text.

It is /guaranteed/ (well, actually, due to lack of formal verification, it is
just /assumed/) that all of the functions and 'SizeTransformer's defined herein
will never break the appropriate properties encoded in their types (as long as
you do not construct your own ones using "TextBox.Internals", of course).

E.g. 'WidthSetter' will never change 'TextBox' properties other than width and
it will always set the width to be equal to the given number, and the result of
'combineProportionally' will never be anything other than 'SizeTransformer'
instance that changes exactly one property of given 'TextBox'.

-}
module TextBox
  (
    TextBox
    -- * Empty box constructors
  , emptyBox
  , emptyBoxByWidth
  , emptyBoxByHeight
    -- * Converters
  , toTextBox
  , toTextBoxWith
  , fromTextBox
    -- * Joiners
  , hJoin
  , hJoinWith
  , vJoin
  , vJoinWith
    -- * Folds with joiners
  , hFold , hFoldWith
  , vFold , vFoldWith
    -- * Size transformers
  , SizeTransformer (unwrapST)
  , WidthTrimmer
  , HeightTrimmer
  , WidthPadder
  , HeightPadder
  , WidthSetter
  , HeightSetter
    -- * Trimmers
  , rightTrimmer
  , leftTrimmer
  , topTrimmer
  , bottomTrimmer
    -- * Padders
  , leftPadder
  , rightPadder
  , centerWidthPadder
  , topPadder
  , bottomPadder
  , centerHeightPadder
    -- * Trimming functions
  , trimTop
  , trimBottom
  , trimRight
  , trimLeft
    -- * Padding functions
  , padTop
  , padBottom
  , padRight
  , padLeft
    -- * Combinators
  , combineProportionally
  , combineEqually
  , mkWidthSetter
  , mkHeightSetter
    -- * Transformers
  , hFlip
  , vFlip
  , transposeBox
    -- * Infix operators
  , (<+|>)
  , (<+|^>)
  , (<+->)
  , (<+-<>)
    -- * String-like class
  , StringLike (..)
  )
where

import TextBox.Data
import TextBox.Utils
import TextBox.Utils.Transform
import TextBox.Internals
import TextBox.Operators
import TextBox.StringLike
