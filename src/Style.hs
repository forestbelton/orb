{-# LANGUAGE OverloadedStrings #-}

module Style where

import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as M
import Text.CSS.Parse
import Data.Text (pack, unpack)
import Data.Attoparsec.Text
import Data.Maybe
import Control.Applicative

data PropKey
    = MarginLeft
    | BorderLeftWidth
    | PaddingLeft
    | Width
    | PaddingRight
    | BorderRightWidth
    | MarginRight
    | BackgroundColor
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
hexParser = digit <|> satisfy isHex 
                        where isHex c = inClass "abcdefABCDEF" c 

-- Parser for color in #RGB format
colorRGBParser :: Parser PropVal
colorRGBParser = do
    char '#'
    r <- hexParser 
    g <- hexParser 
    b <- hexParser 
    return $ Color (SDL.Color (read $ "0x" ++ [r, r]) (read $ "0x" ++ [g, g]) (read $ "0x" ++ [b, b]) 255)

-- Parser for color in #RRGGBB format
colorRRGGBBParser :: Parser PropVal
colorRRGGBBParser = do
    char '#'
    r <- count 2 hexParser 
    g <- count 2 hexParser 
    b <- count 2 hexParser 
    return $ Color (SDL.Color (read $ "0x" ++ r) (read $ "0x" ++ g) (read $ "0x" ++ b) 255)

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
defaults k = maybe (error "unknown property") id $ M.lookup k vals
    where vals = M.fromAscList [
                    (MarginLeft, NumUnit 0 Px)
                  , (BorderLeftWidth, NumUnit 0 Px) -- TODO: replace with medium
                  , (PaddingLeft, NumUnit 0 Px)
                  , (Width, Auto)
                  , (PaddingRight, NumUnit 0 Px)
                  , (BorderRightWidth, NumUnit 0 Px) -- TODO: replace with medium
                  , (MarginRight, NumUnit 0 Px)
                ]
newtype Style = Style (M.Map PropKey PropVal)
  deriving (Show)

parseStyle :: String -> Style
parseStyle s = Style $ M.fromAscList $ catMaybes $ map (\(k, v) -> (,) <$> parseKey (unpack k) <*> parseVal (unpack v)) ((\(Right x) -> x) $ parseAttrs (pack s))

