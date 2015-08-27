{-# LANGUAGE OverloadedStrings #-}

module Style where

import qualified Graphics.UI.SDL as SDL
import qualified Data.Map as M
import Text.CSS.Parse
import Data.Text (pack, unpack)
import Data.Attoparsec.Text
import Data.Word
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
parseKey str = maybeResult (parse keyParser (pack str))

-- Parse the values given in through a String
parseVal :: String -> Maybe PropVal
parseVal str = maybeResult (parse valParser (pack str))

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

-- Parse the colors. This will probably change.
colorParser :: Parser PropVal 
colorParser = 
     (string "red"      >> (return $ Color (SDL.Color 255 0 0 255)))
 <|> (string "green"    >> (return $ Color (SDL.Color 0 255 0 255)))
 <|> (string "blue"     >> (return $ Color (SDL.Color 0 0 255 255)))

-- Parse the values. Both the colors, and the numbers/units
valParser :: Parser PropVal
valParser = numUnitParser <|> colorParser

{-
defaults :: PropKey -> PropVal
defaults k = maybe (error "unknown property") id $ M.lookup k vals
    where vals = M.fromAscList [
                    (MarginLeft, Px 0)
                  , (BorderLeftWidth, Px 0) -- TODO: replace with medium
                  , (PaddingLeft, Px 0)
                  , (Width, Auto)
                  , (PaddingRight, Px 0)
                  , (BorderRightWidth, Px 0) -- TODO: replace with medium
                  , (MarginRight, Px 0)
                ]
-}
newtype Style = Style (M.Map PropKey PropVal)
  deriving (Show)

-- ultimately should look like this:
--
-- M.fromAscList $ catMaybes $ map (\(k, v) -> (,) <$> propKey k <*> propVal v)

{-
parseStyle :: String -> (String, Style)
parseStyle s = case parseBlock $ pack s of
    Right (sel, props) -> (unpack sel, Style $ M.fromAscList $ map (\(k, v) -> (keyParser (unpack k), valParser (unpack v))) props)-}
