{-# LANGUAGE OverloadedStrings #-}
module Style.Parser (parseStyle) where

import Style.Color
import Style.Types

import Control.Applicative
import Data.Char
import Data.Attoparsec.Text
import qualified Data.Map as M
import Data.Maybe
import Data.Text (pack, unpack, Text)
import Text.CSS.Parse

-- Parse the keys given in through a String
parseKey :: String -> Maybe PropKey
parseKey = either (const Nothing) Just . parseOnly (keyParser <* endOfInput) . pack

-- Parse the values given in through a String
parseVal :: String -> Maybe PropVal
parseVal = either (const Nothing) Just . parseOnly (valParser <* endOfInput) . pack

token :: Text -> a -> Parser a
token s x = string s *> pure x

tokens :: [(Text, a)] -> Parser a
tokens = foldr1 (<|>) . map (uncurry token)

-- Parse all possible PropKeys
keyParser :: Parser PropKey
keyParser = tokens [
      ("background-color", BackgroundColor)
    , ("width", Width)
    , ("margin-right", MarginRight)
    , ("margin-left", MarginLeft)
    , ("padding-left", PaddingLeft)
    , ("padding-right", PaddingRight)
    , ("border-left-width", BorderLeftWidth)
    , ("border-right-width", BorderRightWidth)
    ]

-- Parse the units
unitParser :: Parser Units
unitParser = tokens [
      ("px", Px)
    , ("em", Em)
    , ("%", Percent)
    , ("pt", Pt)
    ]

-- Parse the number before the units, and the units themselves
numUnitParser :: Parser PropVal
numUnitParser = NumUnit <$> decimal <*> unitParser

-- Parse a single hex character
hexParser :: Parser Char
hexParser = satisfy isHexDigit

colorHexParser :: Parser String -> Parser PropVal
colorHexParser p = Style.Color.rgb <$> (char '#' *> hp) <*> hp <*> hp
                        where hp = (read . ("0x" ++)) <$> p

-- Parser for color in #RGB format
colorRGBParser :: Parser PropVal
colorRGBParser = colorHexParser (replicate 2 <$> hexParser)

-- Parser for color in #RRGGBB format
colorRRGGBBParser :: Parser PropVal
colorRRGGBBParser = colorHexParser (count 2 hexParser)

-- Parser for color in string literal format
colorStringParser :: Parser PropVal
colorStringParser =
     token "red" red
 <|> token "green" green
 <|> token "blue" blue
 <|> token "yellow" yellow

-- Parse the values. Both the colors, and the numbers/units
valParser :: Parser PropVal
valParser = numUnitParser <|> colorStringParser <|> colorRRGGBBParser <|> colorRGBParser

parseStyle :: String -> Style
parseStyle = Style . M.fromAscList . catMaybes . map (\(k, v) -> (,) <$> parseKey (unpack k) <*> parseVal (unpack v)) . (\(Right x) -> x) . parseAttrs . pack
