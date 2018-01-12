{-|
Module      : TextBox.Utils
Description : Utils for working with textboxes.
Copyright   : (c) klntsky, 2018
License     : PublicDomain
Maintainer  : klntsky@gmail.com
Stability   : experimental
-}
module TextBox.Utils
  (
    -- * Empty box constructors
    emptyBox
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
  )
where


import Data.Ratio (Ratio, numerator, denominator)

import TextBox.Data
import TextBox.Internals
import TextBox.StringLike


{- | Empty 'TextBox' (width and height are both set to 0).
Note that @emptyBox /=@ 'toTextBox' 'empty'.

'emptyBox' is an identity of both 'vJoin' and 'hJoin'. -}
emptyBox :: TextBox a
emptyBox = TextBox 0 0 []


{- | Create a zero-height box with given width. -}
emptyBoxByWidth :: StringLike a => Int -> TextBox a
emptyBoxByWidth n = TextBox n 0 []


{- | Create a zero-width box with given height. -}
emptyBoxByHeight :: StringLike a => Int -> TextBox a
emptyBoxByHeight n = TextBox 0 n (replicate n empty)


{- | Construct a 'TextBox' from 'StringLike' instance. Note that 'toTextBox'
is not a surjection (there is no such @s@ that @toTextBox s ==@ 'emptyBox'). -}
toTextBox :: StringLike a => a -> TextBox a
toTextBox = toTextBoxWith rightPadder


{- | Construct a 'TextBox' from 'StringLike' instance using given
'WidthPadder'. -}
toTextBoxWith :: StringLike a => WidthPadder -> a -> TextBox a
toTextBoxWith wp str =
  let lines = splitLines str in
    vFoldWith wp $ map (\line -> TextBox (lineLength line) 1 [line]) lines


{- | Convert 'TextBox' to the appropriate instance of 'StringLike', e.g.
'String'. -}
fromTextBox :: StringLike a => TextBox a -> a
fromTextBox (TextBox _ _ ls) = joinLines ls


hJoinWith :: StringLike a =>
        HeightPadder
      -> TextBox a
      -> TextBox a
      -> TextBox a
hJoinWith hp b1@(TextBox w1 h1 _) b2@(TextBox w2 h2 _) =
  TextBox (w1 + w2) h $ zipWith append ls1 ls2
  where
    h = max h1 h2
    TextBox _ _ ls1
      | h == h1 = b1
      | otherwise = unwrapST hp h b1
    TextBox _ _ ls2
      | h == h2 = b2
      | otherwise = unwrapST hp h b2


{- | Join boxes horizontally using 'bottomPadder' to adjust the height of the
 lower box. -}
hJoin :: StringLike a =>
        TextBox a
      -> TextBox a
      -> TextBox a
hJoin = hJoinWith bottomPadder


vJoinWith :: StringLike a =>
            WidthPadder
          -> TextBox a
          -> TextBox a
          -> TextBox a
vJoinWith wp b1@(TextBox w1 h1 _) b2@(TextBox w2 h2 _) =
  TextBox w (h1 + h2) (ls1 ++ ls2)
  where
    w = max w1 w2
    TextBox _ _ ls1
      | w == w1 = b1
      | otherwise = unwrapST wp w b1
    TextBox _ _ ls2
      | w == w2 = b2
      | otherwise = unwrapST wp w b2


{- | Join boxes vertically using 'rightPadder' to adjust the width of the
narrower box. -}
vJoin :: StringLike a => TextBox a -> TextBox a -> TextBox a
vJoin = vJoinWith rightPadder


-- | Fold a list of textboxes using 'vJoinWith'.
vFoldWith :: StringLike a => WidthPadder -> [TextBox a] -> TextBox a
vFoldWith wp boxes =
  foldr (vJoinWith wp) (emptyBoxByWidth maxWidth) boxes
  where maxWidth = maximum (map getWidth boxes)


-- | Fold a list of textboxes using 'vJoin'.
vFold :: StringLike a => [TextBox a] -> TextBox a
vFold = vFoldWith rightPadder


-- | Fold a list of textboxes using 'vJoinWith'.
hFoldWith :: StringLike a => HeightPadder -> [TextBox a] -> TextBox a
hFoldWith hp boxes = foldr (hJoinWith hp) (emptyBoxByHeight maxHeight) boxes
  where maxHeight = maximum (map getWidth boxes)


-- | Fold a list of textboxes using 'vJoin'.
hFold :: StringLike a => [TextBox a] -> TextBox a
hFold = foldr hJoin emptyBox


rightTrimmer :: WidthTrimmer
rightTrimmer = MkWidthTrimmer $
  \n box@(TextBox w h ls) -> case () of
    _ | n < w -> TextBox n h $ map (takeN n) ls
      | otherwise -> box


leftTrimmer :: WidthTrimmer
leftTrimmer = MkWidthTrimmer $
  \n box@(TextBox w h ls) -> case () of
    _ | n < w -> TextBox n h $ map (dropN (w - n)) ls
      | otherwise -> box


topTrimmer :: HeightTrimmer
topTrimmer = MkHeightTrimmer $
  \n box@(TextBox w h ls) -> case () of
    _ | n < h -> TextBox w n $ drop (h - n) ls
      | otherwise -> box


bottomTrimmer :: HeightTrimmer
bottomTrimmer = MkHeightTrimmer $
  \n box@(TextBox w h ls) -> case () of
    _ | n < h -> TextBox w n $ take n ls
      | otherwise -> box


{- |
== __Example__

@
hJoinWith topPadder a b =
     +-----+
     |     |
+---+|  B  |
| A ||     |
+---++-----+
@
-}
topPadder :: HeightPadder
topPadder = MkHeightPadder $
  \n r@(TextBox w h ls) -> case () of
    _ | h > n -> r
      | otherwise -> TextBox w n $ mkRect w (n - h) ++ ls

{- |
== __Example__

@
hJoinWith bottomPadder a b =
+---++-----+
| A ||     |
+---+|  B  |
     |     |
     +-----+
@
-}
bottomPadder :: HeightPadder
bottomPadder = MkHeightPadder $
  \n r@(TextBox w h ls) -> case () of
    _ | h > n -> r
      | otherwise -> TextBox w n $ ls ++ (mkRect w (n - h))


{- |
== __Example__

@
hJoinWith centerHeightPadder a b =
     +-----+
+---+|     |
| A ||  B  |
+---+|     |
     +-----+
@
-}
centerHeightPadder :: HeightPadder
centerHeightPadder = combineEqually topPadder bottomPadder


{- |
== __Example__

@
JoinWith leftPadder a b =
  +---+
  | A |
  +---+
+-----+
|     |
|  B  |
|     |
+-----+
@
-}
leftPadder :: WidthPadder
leftPadder = MkWidthPadder $
  \n box@(TextBox w h ls) -> case () of
    _ | w < n -> TextBox n h $ map (mkSpaces (n - w) `append`) ls
      | otherwise -> box


{- |
== __Example__

@
vJoinWith rightPadder a b =
+---+
| A |
+---+
+-----+
|     |
|  B  |
|     |
+-----+
@
-}
rightPadder :: WidthPadder
rightPadder = MkWidthPadder $
  \n box@(TextBox w h ls) -> case () of
    _ | w < n -> TextBox n h $ map (`append` mkSpaces (n - w)) ls
      | otherwise -> box


{- |
== __Example__

@
vJoinWith centerWidthPadder a b =
 +---+
 | A |
 +---+
+-----+
|     |
|  B  |
|     |
+-----+
@
-}
centerWidthPadder :: WidthPadder
centerWidthPadder = combineEqually leftPadder rightPadder



-- | Apply 'rightTrimmer'
trimRight :: StringLike a => Int -> TextBox a -> TextBox a
trimRight = unwrapST rightTrimmer


-- | Apply 'leftTrimmer'
trimLeft :: StringLike a => Int -> TextBox a -> TextBox a
trimLeft = unwrapST leftTrimmer


-- | Apply 'topTrimmer'
trimTop :: StringLike a => Int -> TextBox a -> TextBox a
trimTop = unwrapST topTrimmer


-- | Apply 'bottomTrimmer'
trimBottom :: StringLike a => Int -> TextBox a -> TextBox a
trimBottom = unwrapST bottomTrimmer


-- | Apply 'rightPadder'
padRight :: StringLike a => Int -> TextBox a -> TextBox a
padRight = unwrapST rightPadder


-- | Apply 'leftPadder'
padLeft :: StringLike a => Int -> TextBox a -> TextBox a
padLeft = unwrapST leftPadder


-- | Apply 'topPadder'
padTop :: StringLike a => Int -> TextBox a -> TextBox a
padTop = unwrapST topPadder


-- | Apply 'bottomPadder'
padBottom :: StringLike a => Int -> TextBox a -> TextBox a
padBottom = unwrapST bottomPadder


-- | Construct a 'WidthSetter'.
mkWidthSetter :: WidthPadder -> WidthTrimmer -> WidthSetter
mkWidthSetter = combine


-- | Construct a 'HeightSetter'.
mkHeightSetter :: HeightPadder -> HeightTrimmer -> HeightSetter
mkHeightSetter = combine


{- | Combine 'SizeTransformer's of the same type sharing their work
proportionally.

== __Examples__

@
combineProportionally 1 leftPadder rightPadder
@

defines a padder that centers it's input (see 'centerWidthPadder').

@
combineProportionally (1 % 2) leftPadder rightPadder
@

defines a padder that
centers it's input, but makes padding on the right approximately twice as wide
as on the left.
-}
combineProportionally :: SizeTransformer t p =>
  Ratio Int -- ^ Proportion. 'Ratio' is used to make
            -- sure that the denominator is non-zero.
  -> t -> t -> t
combineProportionally ratio t1 t2 = wrapST $
    \n p -> let
      -- Get current value of the property
      value = getProperty (property t1) p :: Int
      -- Calculate the difference between the actual and the needed value
      diff = difference t1 n value :: Int
      -- Divide it proportionally and apply two transformers sequentially.
      n' = (diff * numerator ratio) `div` (numerator ratio + denominator ratio) in
        unwrapST t1 n $ unwrapST t2 (n - n') p


-- | Defined as @combineProportionally 1@
combineEqually :: SizeTransformer t p => t -> t -> t
combineEqually = combineProportionally 1