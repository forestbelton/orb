{-# LANGUAGE OverloadedStrings #-}

module Style (
      module Style.Types
    , styleNode
) where

import DOM
import Node
import Style.Types
import Style.Color

import Graphics.UI.SDL.TTF.FFI (TTFFont)
import qualified Data.Map as M
import Text.CSS.Parse
import Data.Text (pack, unpack, Text)
import Data.Attoparsec.Text
import Data.Maybe
import Data.Char
import Control.Applicative


-- Parse the keys given in through a String
parseKey :: String -> Maybe PropKey
parseKey = either (const Nothing) Just . parseOnly (keyParser <* endOfInput) . pack 

-- Parse the values given in through a String
parseVal :: String -> Maybe PropVal
parseVal = either (const Nothing) Just . parseOnly (valParser <* endOfInput) . pack

{-
token :: Text a -> a -> Parser a
token s x = string s *> pure x
-}

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
     (string "red"      >> return (Style.Color.rgb 255 0 0))
 <|> (string "green"    >> return (Style.Color.rgb 0 255 0))
 <|> (string "blue"     >> return (Style.Color.rgb 0 0 255))

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

parseStyle :: String -> Style
parseStyle = Style . M.fromAscList . catMaybes . map (\(k, v) -> (,) <$> parseKey (unpack k) <*> parseVal (unpack v)) . (\(Right x) -> x) . parseAttrs . pack

styleNode :: DOMNode -> Node (NodeType, Style)
styleNode (Node nt cs) = Node (nt, sty) $ map styleNode cs
    where sty = case nt of
                    Element _ attrs -> maybe (Style M.empty) parseStyle $ M.lookup "style" attrs
                    _               -> Style M.empty
