{-# LANGUAGE OverloadedStrings #-}
module Style.Parser where

import Prelude hiding (takeWhile)
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

sepByN :: Int -> Parser a -> Parser b -> Parser [a]
sepByN n p delim = (:) <$> p <*> t
    where t = count (n - 1) (delim *> p)

term :: Text -> Parser ()
term s = string s *> takeWhile (inClass " \r\t\n") *> pure ()

colorClipInt :: Integral a => Parser a
colorClipInt = fromIntegral . max 0 . min 255 <$> signed decimal

colorClipPct :: Integral a => Parser a
colorClipPct = round . max 0 . min 255 . (* (255 / 100)) <$> (double <* char '%')

colorRGBFuncParser :: Parser PropVal
colorRGBFuncParser = buildCol <$> (term "rgb(" *> (commaSep colorClipInt <|> commaSep colorClipPct) <* (term ")"))
  where commaSep p = sepByN 3 p (term ",")
        buildCol [x, y, z] = rgb x y z

-- Parse the values. Both the colors, and the numbers/units
valParser :: Parser PropVal
valParser = numUnitParser <|> colorStringParser <|> colorRRGGBBParser <|> colorRGBParser <|> colorRGBFuncParser

parseStyle :: String -> Style
parseStyle = Style . M.fromAscList . catMaybes . map (\(k, v) -> (,) <$> parseKey (unpack k) <*> parseVal (unpack v)) . (\(Right x) -> x) . parseAttrs . pack
