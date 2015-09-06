module Layout where

import Types
import DOM
import Paint
import Style
import qualified Style.Lookup as S
import qualified Data.Map as M
import qualified Graphics.UI.SDL as SDL
import Foreign.C.Types
import Control.Lens hiding (children)

import Layout.Types
import Layout.Height
import Layout.Width
import Layout.Box

-- todo: make safer
findPx :: Style -> PropKey -> CInt
findPx sty = fromIntegral . fromPx . S.lookup sty

buildLayout' :: Int -> Int -> BoxNode -> LayoutNode
buildLayout' parentWidth y node = LayoutNode (node ^. nodeType) (node ^. style) lay cs'
    where lay = Layout $ SDL.Rect 0 (fromIntegral y) (fromIntegral w) h
          w = width parentWidth node
          h = fromIntegral $ y' - y
          (y', cs') = foldr go (y, []) (reverse $ node ^. children) -- todo: figure out why i have to reverse here
          go c (y1, cs1) = (y1 + height c, buildLayout' w y1 c : cs1)

buildLayout :: BoxNode -> LayoutNode
buildLayout = buildLayout' 800 0

buildDisplayCommands :: LayoutNode -> [DisplayCommand]
buildDisplayCommands node = dc : concatMap buildDisplayCommands (node ^. children)
    where sty = node ^. style
          lay = layoutContent $ node ^. layout
          dc = case node ^. nodeType of
                   Element _ _ -> displayRect (fromColor $ S.lookup sty BackgroundColor) lay
                   Text s -> case lay of
                       (SDL.Rect x y _ _ ) -> displayText x y (fromFont $ S.lookup sty FontFamily) (fromColor $ S.lookup sty FontColor) s

layoutNode :: StyledNode -> [DisplayCommand]
layoutNode = buildDisplayCommands . buildLayout . makeAnonymousBox . makeBoxModel
