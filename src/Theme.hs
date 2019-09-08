module Theme
    ( theme
    )
where

import           Graphics.Vty
import qualified Brick.Widgets.List            as List
import           Brick.AttrMap                  ( attrName )
import           Brick.Util
import           Brick.Themes                   ( Theme
                                                , newTheme
                                                )

theme :: Theme
theme = newTheme
    (white `on` black)
    [ (List.listAttr               , fg white)
    , (List.listSelectedAttr       , fg brightWhite)
    , (List.listSelectedFocusedAttr, black `on` red)
    , (attrName "key"              , withStyle (fg green) bold)
    , (attrName "bold"             , withStyle (fg white) bold)
    , (attrName "current"          , fg brightWhite)
    , (attrName "title"            , withStyle (fg brightRed) bold)
    ]

