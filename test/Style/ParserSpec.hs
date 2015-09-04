module Style.ParserSpec (spec) where

import qualified Data.Map as M
import Test.Hspec

import Style.Color
import Style.Types
import Style.Parser

style :: [(PropKey, PropVal)] -> Style
style = Style . M.fromAscList

spec :: Spec
spec = do
    describe "parseStyle" $ do
        it "can parse colors" $ do
            let testRed s = parseStyle s `shouldBe` style [(BackgroundColor, red)]
            mapM_ testRed [
                 "background-color: red;"
               , "background-color: #f00;"
               , "background-color: #ff0000;"
             ]

        it "can parse pixels" $ do
            parseStyle "width: 500px" `shouldBe` style [(Width, NumUnit 500 Px)]

        it "can parse percentages" $ do
            parseStyle "width: 100%" `shouldBe` style [(Width, NumUnit 100 Percent)]
