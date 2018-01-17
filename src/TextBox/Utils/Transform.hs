module TextBox.Utils.Transform
  (
    hFlip
  , vFlip
  , transposeBox
  )
where

import TextBox.Data
import TextBox.Utils
import TextBox.StringLike

import Data.List (transpose)


-- | Transpose.
transposeBox :: StringLike a => TextBox a -> TextBox a
transposeBox (TextBox w h lst)
  | h == 0 = emptyBoxByHeight w
  | w == 0 = emptyBoxByWidth h
  | otherwise =
      TextBox h w . map (foldr append empty) .
      transpose . map toList $ lst


-- | Flip horizontally.
hFlip :: StringLike a => TextBox a -> TextBox a
hFlip (TextBox w h lst) = TextBox w h $ map inverse lst


-- | Flip vertically.
vFlip :: StringLike a => TextBox a -> TextBox a
vFlip (TextBox w h lst) = TextBox w h $ reverse lst
