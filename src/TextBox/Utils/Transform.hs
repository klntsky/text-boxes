module TextBox.Utils.Transform
  (
    hFlip
  , vFlip
  , transposeBox
  )
where

import TextBox.Data
import TextBox.StringLike


-- | Transpose.
transposeBox :: StringLike a => TextBox a -> TextBox a
transposeBox (TextBox w h lst) = TextBox h w . reverse $ map inverse lst


-- | Flip horizontally.
hFlip :: StringLike a => TextBox a -> TextBox a
hFlip (TextBox w h lst) = TextBox w h $ map inverse lst


-- | Flip vertically.
vFlip :: StringLike a => TextBox a -> TextBox a
vFlip (TextBox w h lst) = TextBox w h $ reverse lst
