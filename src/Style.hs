{-# LANGUAGE OverloadedStrings #-}

module Style where

import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as M
import Text.CSS.Parse
import Data.Text (pack, unpack, Text)
import Data.Attoparsec.Text
import Data.Maybe
import Data.Char
import Control.Applicative

data PropKey
    = MarginTop
    | MarginRight
    | MarginBottom
    | MarginLeft
    | Width
    | BorderTopWidth
    | BorderRightWidth
    | BorderLeftWidth
    | BorderBottomWidth
    | BackgroundColor
    | PaddingTop
    | PaddingRight
    | PaddingBottom
    | PaddingLeft
  deriving (Eq, Ord, Show)

data Units
    = Px
    | Em
    | Percent
    | Pt
    deriving (Eq, Show)

data PropVal
    = NumUnit Int Units
    | Color SDL.Color
    | Auto
    | None 
  deriving (Eq, Show)

-- Parse the keys given in through a String
parseKey :: String -> Maybe PropKey
parseKey = either (const Nothing) Just . parseOnly (keyParser <* endOfInput) . pack 

-- Parse the values given in through a String
parseVal :: String -> Maybe PropVal
parseVal = either (const Nothing) Just . parseOnly (valParser <* endOfInput) . pack

token :: Text a -> a -> Parser a
token s x = string s *> pure x

-- Parse all possible PropKeys
keyParser :: Parser PropKey
keyParser = 
     (string "background-color"     >> return BackgroundColor)
 <|> (string "width"                >> return Width)
 <|> (string "margin-right"         >> return MarginRight)
 <|> (string "margin-left"          >> return MarginLeft)
 <|> (string "padding-left"         >> return PaddingLeft)
 <|> (string "padding-right"        >> return PaddingRight)
 <|> (string "border-left-width"    >> return BorderLeftWidth)
 <|> (string "border-right-width"   >> return BorderRightWidth)

-- Parse the units
unitParser :: Parser Units
unitParser = 
     (string "px"       >> return Px)
 <|> (string "em"       >> return Em)
 <|> (string "%"        >> return Percent)
 <|> (string "pt"       >> return Pt)

-- Parse the number before the units, and the units themselves
numUnitParser :: Parser PropVal 
numUnitParser = NumUnit <$> decimal <*> unitParser

-- Parse a single hex character
hexParser :: Parser Char
hexParser = satisfy isHexDigit

colorHexParser :: Parser String -> Parser PropVal
colorHexParser p = Color <$> (SDL.Color <$> (char '#' *> hp) <*> hp <*> hp <*> pure 255)
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
     (string "red"      >> return (Color (SDL.Color 255 0 0 255)))
 <|> (string "green"    >> return (Color (SDL.Color 0 255 0 255)))
 <|> (string "blue"     >> return (Color (SDL.Color 0 0 255 255)))

-- Parse the values. Both the colors, and the numbers/units
valParser :: Parser PropVal
valParser = numUnitParser <|> colorStringParser <|> colorRRGGBBParser <|> colorRGBParser

defaults :: PropKey -> PropVal
defaults = const (NumUnit 0 Px)

{-
defaults :: PropKey -> PropVal
defaults k = maybe (error "unknown property") id $ M.lookup k vals
    where vals = M.fromAscList [
                    (MarginLeft, NumUnit 0 Px)
                  , (BorderLeftWidth, NumUnit 0 Px) -- TODO: replace with medium
                  , (PaddingLeft, NumUnit 0 Px)
                  , (Width, Auto)
                  , (PaddingRight, Px 0)
                  , (BorderRightWidth, Px 0) -- TODO: replace with medium
                  , (BorderTopWidth, Px 0) -- TODO: replace with medium
                  , (BorderRightWidth, Px 0) -- TODO: replace with medium
                  , (MarginRight, Px 0)
                ]
-}

newtype Style = Style (M.Map PropKey PropVal)
  deriving (Show)

parseStyle :: String -> Style
parseStyle = Style . M.fromAscList . catMaybes . map (\(k, v) -> (,) <$> parseKey (unpack k) <*> parseVal (unpack v)) . (\(Right x) -> x) . parseAttrs . pack

