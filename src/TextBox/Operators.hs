{-|
Module      : TextBox.Operators
Description : Infix operators.
Copyright   : (c) klntsky, 2018
License     : PublicDomain
Maintainer  : klntsky@gmail.com
Stability   : experimental

Infix synonyms for joins.
-}
module TextBox.Operators
  (
    (<+|>)
  , (<+|^>)
  , (<+->)
  , (<+-<>)
  )
where

import TextBox.StringLike (StringLike)
import TextBox.Data (TextBox)
import TextBox.Utils
  (
    hJoin
  , hJoinWith
  , vJoin
  , vJoinWith
  , topPadder
  , leftPadder
  )


(<+|>), (<+|^>), (<+->), (<+-<>) :: StringLike a => TextBox a -> TextBox a -> TextBox a


-- | Infix synonym to 'hJoin'
a <+|> b = hJoin a b
infixl 6 <+|>


-- | Infix synonym to 'hJoinWith' 'topPadder'
a <+|^> b = hJoinWith topPadder a b
infixl 6 <+|^>


-- | Infix synonym to 'vJoin'
a <+-> b = vJoin a b
infixl 6 <+->


-- | Infix synonym to 'vJoinWith' 'leftPadder'
a <+-<> b = vJoinWith leftPadder a b
infixl 6 <+-<>
