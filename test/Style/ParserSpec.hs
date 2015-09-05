module Style.ParserSpec (spec) where

import qualified Data.Map as M
import Test.Hspec

import Style.Color
import Style.Types
import Style.Parser

style :: [(PropKey, PropVal)] -> Style
style = Style . M.fromAscList

testRed s = parseStyle ("background-color:" ++ s) `shouldBe` style [(BackgroundColor, red)]

spec :: Spec
spec = do
    describe "parseStyle" $ do
        describe "colors" $ do
            mapM_ (\(name, val) -> it name $ testRed val) [
                ("can parse color names", "red")
              , ("can parse three-digit hex codes", "#f00")
              , ("can parse six-digit hex codes", "#ff0000")
              , ("can parse rgb() values", "rgb(255,0,0)")
              , ("can parse rgb() values clipped above", "rgb(300,0,0)")
              , ("can parse rgb() values clipped below", "rgb(255,-10,0)")
              , ("can parse rgb() values with percentages", "rgb(110%, 0%, 0%)")
              ]

            it "can't parse rgb() values with mixed units" $ do
                parseStyle "background-color:rgb(0,100%,7)" `shouldBe` style []

        it "can parse pixels" $ do
            parseStyle "width: 500px" `shouldBe` style [(Width, NumUnit 500 Px)]

        it "can parse percentages" $ do
            parseStyle "width: 100%" `shouldBe` style [(Width, NumUnit 100 Percent)]
