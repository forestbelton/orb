{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import Control.Lens.TH
import Style.Types
import Layout.Types
import qualified Data.Map as M

data NodeType
    = Text String
    | Element String (M.Map String String)
    | Comment String
    deriving (Eq, Show)

data DomNode = DomNode {
    _domNodeNodeType :: NodeType,
    _domNodeChildren :: [DomNode]
  } deriving (Eq, Show)
makeFields ''DomNode

data StyledNode = StyledNode {
    _styledNodeNodeType :: NodeType,
    _styledNodeStyle :: Style,
    _styledNodeChildren :: [StyledNode]
  } deriving (Eq, Show)
makeFields ''StyledNode

data BoxNode = BoxNode {
    _boxNodeNodeType :: NodeType,
    _boxNodeStyle :: Style,
    _boxNodeBoxType :: BoxType,
    _boxNodeChildren :: [BoxNode]
  } deriving (Eq, Show)
makeFields ''BoxNode

data LayoutNode = LayoutNode {
    _layoutNodeNodeType :: NodeType,
    _layoutNodeStyle :: Style,
    _layoutNodeLayout :: Layout,
    _layoutNodeChildren :: [LayoutNode]
  } deriving (Eq, Show)
makeFields ''LayoutNode
