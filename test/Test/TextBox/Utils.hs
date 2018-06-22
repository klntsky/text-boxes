{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Test.TextBox.Utils (spec) where


import Test.Hspec
import TextBox
import TextBox.StringLike
import TextBox.StringLike.String
import TextBox.Utils
import TextBox.Data
import Test.QuickCheck
import qualified Test.QuickCheck as QC
import Data.Ratio

spec :: Spec
spec = do

  describe "fromTextBox . toTextBox" $ do
    it "must pad strings properly" $
      map (fromTextBox . toTextBox)
      [
        "\na"
      , "a"
      , "123\n1"
      ] `shouldBe`
      [
        " \na"
      , "a"
      , "123\n1  "
      ]

  -- trimmers

  describe "trimRight" $ do
    it "is implemented correctly" $
      map (\(s, n) -> trimRight n (toTextBox s))
      [
        ("a", 1)
      , ("a", 0)
      , ("ab", 1)
      , ("abc\ndef", 2)
      , ("abcd\nef", 3)
      , ("1234", 0)
      , ("1234", 2)
      , ("1234", 4)
      , ("1234\n5", 1)
      , ("1234\n5", 2)
      , ("1234", 5)
      ] `shouldBe`
      map toTextBox
      [
        "a"
      , ""
      , "a"
      , "ab\nde"
      , "abc\nef "
      , ""
      , "12"
      , "1234"
      , "1\n5"
      , "12\n5 "
      , "1234"
      ]

  describe "trimLeft" $ do
    it "is implemented correctly" $
      map (\(str, n) -> trimLeft n $ toTextBox str)
      [
        ("a", 1)
      , ("a", 0)
      , ("ab", 1)
      , ("abc\ndef", 2)
      , ("abcd\nef", 3)

      ,  ("1234", 0)
      , ("1234", 2)
      , ("1234", 4)
      , ("1234\n5", 1)
      , ("1234\n5", 2)
      , ("1234", 5)
      ] `shouldBe`
      map toTextBox
      [
        "a"
      , ""
      , "b"
      , "bc\nef"
      , "bcd\nf  "

      , ""
      , "34"
      , "1234"
      , "4\n "
      , "34\n  "
      , "1234"
      ]

  describe "trimTop" $ do
    it "is implemented correctly" $
      map (\(str, n) -> trimTop n $ toTextBox str)
      [
        ("1", 0)
      , ("1\n2", 1)
      , ("123", 10)
      ] `shouldBe`
      emptyBoxByWidth 1 :
      map toTextBox
      [
        "2"
      , "123"
      ]



  -- padders



  -- joiners

  describe "hJoin" $ do
    it "'emptyBox' is identity of 'hJoin'" $
      ((emptyBox :: TextBox String) `hJoin` emptyBox) `shouldBe` emptyBox
    it "'emptyBox' is identity of 'hJoin'" $ QC.property $
      \(s :: String) ->
        ( emptyBox `hJoin` (toTextBox s)
        , (toTextBox s) `hJoin` emptyBox ) `shouldBe`
        ( toTextBox s
        , toTextBox s)


    it "is implemented correctly" $
      map (\(a, b) -> fromTextBox (hJoin (toTextBox a) (toTextBox b)))
      [
        ("a", "b")
      , ("a\nb", "c\nd")
      , ("a\n", "b\nc")
      , ("\na", "b")
      ] `shouldBe`
      [
        "ab"
      , "ac\nbd"
      , "ab\n c"
      , " b\na "
      ]

  describe "vJoin" $ do
    it "'emptyBox' is identity of 'vJoin'" $
      ((emptyBox :: TextBox String) `vJoin` emptyBox) `shouldBe` emptyBox
    it "'emptyBox' is identity of 'vJoin'" $ QC.property $
      \(s :: String) ->
        ( emptyBox `vJoin` (toTextBox s)
        , (toTextBox s) `vJoin` emptyBox ) `shouldBe`
        ( toTextBox s
        , toTextBox s)

    it "is implemented correctly" $
      map (\(a, b) -> fromTextBox (vJoin (toTextBox a) (toTextBox b)))
      [
        ("a", "b")
      , ("a\nb", "c\nd")
      , ("a\n", "b\nc")
      , ("\na", "b")
      , ("111\n22", "3")
      , (empty, empty)
      ] `shouldBe`
      [
        "a\nb"
      , "a\nb\nc\nd"
      , "a\n \nb\nc"
      , " \na\nb"
      , "111\n22 \n3  "
      , "\n"
      ]

  describe "combineProportionally" $ do

    it "passes tests for 'WidthPadder's" $
        sequence_ $
          map (\(ratio, a, n, c) ->
                 let padder :: WidthPadder
                     padder = combineProportionally ratio leftPadder rightPadder in
                   fromTextBox (unwrapST padder n (toTextBox a)) `shouldBe` c)
          [
            (1 % 2, "a", 1, "a")
          , (1 % 2, "a", 2, "a ")
          , (1 % 2, "a", 3, " a ")
          , (1 % 2, "a", 4, " a  ")
          , (1 % 2, "a", 7, "   a   ")

          , (1 % 2, "a", 0, "a")
          ]

    it "passes tests for 'HeightPadder's" $
        sequence_ $
          map (\(ratio, a, n, c) ->
                 let padder :: HeightPadder
                     padder = combineProportionally ratio topPadder bottomPadder in
                  fromTextBox (unwrapST padder n (toTextBox a)) `shouldBe` c)
          [
            (1 % 2, "a", 1, "a")
          , (1 % 2, "a", 2, "a\n ")
          , (1 % 2, "a", 3, " \na\n ")
          , (1 % 2, "a", 4, " \na\n \n ")
          , (1 % 2, "a", 5, " \n \na\n \n ")

          , (1 % 2, "a", 0, "a")
          ]

    it "passes tests for 'WidthTrimmer's" $
        sequence_ $
          map (\(ratio, a, n, c) ->
                  let trimmer :: WidthTrimmer
                      trimmer = combineProportionally ratio leftTrimmer rightTrimmer in
                 fromTextBox (unwrapST trimmer n (toTextBox a)) `shouldBe` c)
          [
            (1 % 2, "a", 1, "a")
          , (1 % 2, "ab", 1, "b")
          , (1 % 2, "abc", 1, "b")
          , (1 % 2, "abc", 0, "")

          , (1 % 2, "abc", 10, "abc")

          , (1,  "abc", 1, "c")
          , (0,  "abc", 1, "a")
          ]

    it "passes tests for 'WidthSetter's" $
        sequence_ $
          map (\(ratio, a, n, c) ->
                 let setter, setter1, setter2 :: WidthSetter
                     setter1 = mkWidthSetter leftPadder rightTrimmer
                     setter2 = mkWidthSetter rightPadder leftTrimmer
                     setter = combineProportionally ratio setter1 setter2 in
                   fromTextBox (unwrapST setter n (toTextBox a)) `shouldBe` c)
          [
            (1 % 2, "a", 1, "a")
          , (1 % 2, "a", 2, "a ")
          , (1 % 2, "a", 3, " a ")
          , (1 % 2, "a", 4, " a  ")
          , (1 % 2, "a", 5, "  a  ")
          , (1 % 2, "abcde", 1, "c")
          , (1 % 2, "abcde", 3, "bcd")
          ]

  describe "combineConditionally" $ do
    it "passes tests" $
      sequence_ $
      let predicate :: TextBox a -> Bool
          predicate box = getWidth box > 3 in
      map (\(t1, t2, n, input, expected) ->
             let t = combineConditionally predicate t1 t2 in
               (fromTextBox $ unwrapST t n $ toTextBox input) `shouldBe` expected)
      [
        (leftPadder, rightPadder, 5, "asd", "asd  ")
      , (leftPadder, rightPadder, 5, "asdf", " asdf")
      ]
