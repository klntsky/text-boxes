{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.TextBox.StringLike.Text (spec) where


import Test.Hspec
import TextBox.StringLike
import TextBox.StringLike.Text
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = do

  describe "'StringLike' rules must be satisfied by Text instance" $ do
    it "width empty == 0" $
      width (empty :: Text) `shouldBe` 0
    it "height empty == 1" $
      height (empty :: Text) `shouldBe` 1
    it "width space == 1" $
      width (space :: Text) `shouldBe` 1
    it "height space == 1" $
      height (space :: Text) `shouldBe` 1
    it "height newline == 2" $
      height (newline :: Text) `shouldBe` 2
    it "map height . splitLines == map (const 1) . splitLines" $ property $
      \(x' :: String) -> let x = T.pack x' in
        map height (splitLines x) == map (const 1) (splitLines x)

  describe "'StringLike' instance for 'Text'" $ do
    it "implements 'width' correctly" $
      map (width :: Text -> Int) [
        "123"
      , ""
      , "12\n345"
      ] `shouldBe`
      [
        3
      , 0
      , 3
      ]

    it "implements 'height' correctly" $
      map (height :: Text -> Int)
      [ ""
      , "1"
      , "1\n2"
      , "\n"
      ] `shouldBe`
      [ 1
      , 1
      , 2
      , 2
      ]

    it "'dropN' Must return 'empty' if the length is less than n." $
      map (uncurry (dropN :: Int -> Text -> Text))
      [
        (10, "123456789")
      ]
      `shouldBe`
      [
        empty
      ]

  describe "'splitLines' and 'joinLines' in 'StringLike' instance for 'String'" $ do
    it "must satisfy joinLines . splitLines = id (quickcheck)" $ property $
      \(x' :: String) -> let x = T.pack x' in
        (joinLines . splitLines) x == x

    it "must satisfy joinLines . splitLines = id" $
      map (joinLines . splitLines :: Text -> Text)
      [
        "\n"
      ] `shouldBe`
      [
        "\n"
      ]

  describe "toList" $ do
    it "is implemented correctly" $
      map (toList :: Text -> [Text])
      [
        ""
      , "1"
      , "123"
      ] `shouldBe`
      [
        []
      , ["1"]
      , ["1", "2", "3"]
      ]
