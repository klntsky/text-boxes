{-# LANGUAGE ScopedTypeVariables #-}
module Test.TextBox.StringLike.String (spec) where


import Test.Hspec
import TextBox.StringLike
import TextBox.StringLike.String
import Test.QuickCheck


spec :: Spec
spec = do

  describe "'StringLike' rules must be satisfied by String instance" $ do
    -- does not catch edge case with x = "\n"!
    it "width empty == 0" $
      width (empty :: String) `shouldBe` 0
    it "height empty == 1" $
      height (empty :: String) `shouldBe` 1
    it "width space == 1" $
      width (space :: String) `shouldBe` 1
    it "height space == 1" $
      height (space :: String) `shouldBe` 1
    it "height newline == 2" $
      height (newline :: String) `shouldBe` 2
    it "map height . splitLines == map (const 1) . splitLines" $ property $
      \(x :: String) -> map height (splitLines x) == map (const 1) (splitLines x)

  describe "'StringLike' instance for 'String'" $ do
    it "implements 'width' correctly" $
      (
        width "123"
      , width ""
      , width "12\n345"
      ) `shouldBe`
      (
        3
      , 0
      , 3
      )

    it "implements 'height' correctly" $
      (
        height ""
      , height "1"
      , height "1\n2"
      , height "\n"
      ) `shouldBe`
      (
        1
      , 1
      , 2
      , 2
      )

    it "'dropN' Must return 'empty' if the length is less than n." $
      map (uncurry dropN)
      [
        (10, "123456789")
      ]
      `shouldBe`
      [
        empty
      ]

  describe "'splitLines' and 'joinLines' in 'StringLike' instance for 'String'" $ do
    -- does not catch edge case with x = "\n"!
    it "must satisfy joinLines . splitLines = id (quickcheck)" $ property $
      \(x :: String) -> (joinLines . splitLines) x == x

    it "must satisfy joinLines . splitLines = id" $
      map (joinLines . splitLines)
      [
        "\n"
      ] `shouldBe`
      [
        "\n"
      ]
