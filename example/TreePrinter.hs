{-# LANGUAGE DeriveFunctor #-}
module Test.TreePrinter where

import Data.List (intersperse)
import TextBox


data Tree a = Node a [Tree a]
  deriving Functor


instance StringLike a => Show (Tree a) where
  show = toString . fromTextBox . boxify . fmap toTextBox
    where
      boxify :: StringLike a => Tree (TextBox a) -> TextBox a
      boxify (Node a ns) =
        vJoinWith centerWidthPadder a . hFold .
        intersperse (toTextBox space) $ map boxify ns


tree1 = Node "a" [ Node "b" []
                 , Node "c" []
                 , Node "d" [ Node "f" []
                            , Node "g" []
                            , Node "e" []]]


-- | Output is:
--
-- @
--     a
-- b c   d
--     f g e
-- @
main = print tree1
