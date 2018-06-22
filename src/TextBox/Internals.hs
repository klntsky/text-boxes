{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-|
Module      : TextBox.Internals
Description : Internals of the library.
Copyright   : (c) klntsky, 2018
License     : PublicDomain
Maintainer  : klntsky@gmail.com
Stability   : experimental

Internals of the library. -}
module TextBox.Internals
  (
    -- * TextBox properties
    Width
  , Height
  , TextBoxProperty (..)
    -- * Size transformers
  , SizeTransformer (..)
  , WidthTrimmer (..)
  , HeightTrimmer (..)
  , WidthPadder (..)
  , HeightPadder (..)
  , WidthSetter (..)
  , HeightSetter (..)
  )
where


import TextBox.StringLike
import TextBox.Data


-- | Name for width property.
data Width = Width


-- | Name for height property.
data Height = Height


-- | Type-level wrappers for 'TextBox' property getters.
class TextBoxProperty p where
  getProperty :: StringLike a => p -> TextBox a -> Int


instance TextBoxProperty Width where
  getProperty = const getWidth


instance TextBoxProperty Height where
  getProperty = const getHeight


{- | Typeclass for wrapping, unwrapping and combining functions that change the
'TextBox' size. You need to import "TextBox.Internals" in order to define your
own transformers.

Each size transformer is uniquely associated with some 'TextBoxProperty'.
-}
class TextBoxProperty p => SizeTransformer t p | t -> p where
  -- | Wrap a raw function into transformer.
  wrapST :: (forall a . StringLike a => Int -> TextBox a -> TextBox a) -> t
  -- | Unwrap a transformer.
  --
  -- 'wrapST' and 'unwrapST' must form a bijection.
  --
  -- Transformers defined in 'TextBox.Utils' have the corresponding functions
  -- to run them directly, e.g. 'TextBox.Utils.trimRight' @ = unwrapST@
  -- 'TextBox.Utils.rightTrimmer'
  unwrapST :: t -> (forall a . StringLike a => Int -> TextBox a -> TextBox a)
  -- | Combine two 'SizeTransformer's that change the same property.
  combine :: (SizeTransformer a p, SizeTransformer b p) => a -> b -> t
  combine a b = wrapST (\n -> unwrapST a n . unwrapST b n)
  -- | Get the property that given 'SizeTransformer' may change.
  property :: t -> p
  difference :: t -> Int -> Int -> Int


-- | 'SizeTransformer' that sets the width to be less than or equal to given number.
newtype WidthTrimmer  = MkWidthTrimmer  (forall a . StringLike a => Int -> TextBox a -> TextBox a)
-- | 'SizeTransformer' that sets the height to be less than or equal to given number.
newtype HeightTrimmer = MkHeightTrimmer (forall a . StringLike a => Int -> TextBox a -> TextBox a)
-- | 'SizeTransformer' that sets the width to be greater than or equal to given number.
newtype WidthPadder   = MkWidthPadder   (forall a . StringLike a => Int -> TextBox a -> TextBox a)
-- | 'SizeTransformer' that sets the height to be greater than or equal to given number.
newtype HeightPadder  = MkHeightPadder  (forall a . StringLike a => Int -> TextBox a -> TextBox a)
-- | 'SizeTransformer' that sets the width to be equal to given number.
newtype WidthSetter   = MkWidthSetter   (forall a . StringLike a => Int -> TextBox a -> TextBox a)
-- | 'SizeTransformer' that sets the height to be equal to given number.
newtype HeightSetter  = MkHeightSetter  (forall a . StringLike a => Int -> TextBox a -> TextBox a)


instance SizeTransformer WidthTrimmer Width where
  wrapST = MkWidthTrimmer
  unwrapST (MkWidthTrimmer f) = f
  property = const Width
  difference = \_ needed actual ->
                 if actual > needed then
                   needed - actual
                 else 0

instance SizeTransformer HeightTrimmer Height where
  wrapST = MkHeightTrimmer
  unwrapST (MkHeightTrimmer f) = f
  property = const Height
  difference = \_ needed actual ->
                 if actual > needed then
                   needed - actual
                 else 0

instance SizeTransformer WidthPadder Width where
  wrapST = MkWidthPadder
  unwrapST (MkWidthPadder f) = f
  property = const Width
  difference = \_ needed actual ->
                if needed > actual then
                  needed - actual
                else 0

instance SizeTransformer HeightPadder Height where
  wrapST = MkHeightPadder
  unwrapST (MkHeightPadder f) = f
  property = const Height
  difference = \_ needed actual ->
                if needed > actual then
                  needed - actual
                else 0

instance SizeTransformer WidthSetter Width where
  wrapST = MkWidthSetter
  unwrapST (MkWidthSetter f) = f
  property = const Width
  difference = const (-)

instance SizeTransformer HeightSetter Height where
  wrapST = MkHeightSetter
  unwrapST (MkHeightSetter f) = f
  property = const Height
  difference = const (-)
