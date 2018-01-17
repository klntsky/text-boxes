module Test.TextBox.Utils.Transform (spec) where

import Test.Hspec
import TextBox.StringLike
import TextBox.StringLike.String
import TextBox

spec :: Spec
spec = do
  describe "transposeBox" $ do
    it "passes assertion checks" $
      map (transposeBox) (map fst transposeTests) `shouldBe` map snd transposeTests
    it "satisfies transposeBox . transposeBox = id" $
      let boxes = map fst transposeTests in
        map (transposeBox . transposeBox) boxes `shouldBe` boxes

transposeTests = [ (toTextBox "",         emptyBoxByWidth 1)
                 , (emptyBoxByWidth 0,    emptyBoxByHeight 0)
                 , (emptyBoxByHeight 0,   emptyBoxByWidth 0)
                 , (emptyBoxByWidth 10,   emptyBoxByHeight 10)
                 , (emptyBoxByHeight 10,  emptyBoxByWidth 10)
                 , (toTextBox "a",        toTextBox "a")
                 , (toTextBox "a\nb",     toTextBox "ab")
                 , (toTextBox "ab",       toTextBox "a\nb")
                 , (toTextBox "ab\ncd",   toTextBox "ac\nbd")
                 , (toTextBox "abc\ndef\nghi",   toTextBox "adg\nbeh\ncfi")
                 , (emptyBox,             emptyBox)
                 ]
