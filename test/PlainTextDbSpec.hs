module PlainTextDbSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import PlainTextDb

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

instance Arbitrary CellStyle where
  arbitrary = oneof [return Plain, return Underlined]

instance Arbitrary Cell where
  arbitrary = do
    style <- arbitrary
    contents <- arbitrary
    Positive width <- arbitrary
    return $ Cell style contents width

instance Arbitrary TextRow where
  arbitrary = do
    Positive indent <- arbitrary
    cells <- arbitrary
    return $ TextRow indent cells

spec :: Spec
spec = do
  describe "validate" $
    it "fails if the width of all rows are not equal" $ property $
        \(r1,r2) -> rowWidth r1 /= rowWidth r2 ==> validate [r1, r2] == Just UnequalRows
  describe "formatCell" $ do
    it "always begins a plain cell with a space" $ property $
        \cell -> style cell == Plain ==> head (formatCell cell) == ' '
    it "always ends a plain cell with a space" $ property $
        \cell -> style cell == Plain ==> last (formatCell cell) == ' '
    it "always begins an underlined cell with an underscore" $ property $
        \cell -> style cell == Underlined ==> head (formatCell cell) == '_'
    it "always ends an underlined cell with an underscore" $ property $
        \cell -> style cell == Underlined ==> last (formatCell cell) == '_'
  describe "format" $
    it "generates one row of output for each row in the table" $ property $
        forAll arbitrary $ \txttbl -> length (format txttbl) == length txttbl
