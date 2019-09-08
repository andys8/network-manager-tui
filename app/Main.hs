{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where



import qualified Graphics.Vty                  as V
import qualified Brick.Main                    as M
import           Brick.Types                    ( Widget )
import           Brick.Themes                   ( themeToAttrMap )
import qualified Brick.Types                   as T
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Center          as C
import           Brick.Widgets.Core             ( hLimit
                                                , str
                                                , vBox
                                                , hBox
                                                , padAll
                                                , padLeft
                                                , padRight
                                                , withAttr
                                                , withBorderStyle
                                                , (<+>)
                                                )
import qualified Brick.Widgets.List            as L
import qualified Data.Vector                   as Vec
import           Data.Maybe

import           Lib
import           Theme                          ( theme )


type State = L.List () Connection


main :: IO ()
main = do
  connections <- listConnections
  finalState  <- M.defaultMain app (initialState connections)
  putStrLn =<< toggle finalState


toggle :: State -> IO String
toggle state =
  Data.Maybe.fromMaybe (pure "Something went wrong")
    $ (toggleConnection . snd <$> L.listSelectedElement state)


app :: M.App State e ()
app = M.App { M.appDraw         = appDraw
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent  = appHandleEvent
            , M.appStartEvent   = return
            , M.appAttrMap      = const $ themeToAttrMap theme
            }


appDraw :: State -> [Widget ()]
appDraw state =
  [ C.vCenter $ padAll 1 $ vBox
      [hBox [C.hCenter $ drawBranchList state], str " ", instructions]
  ]
 where
  instructions = C.hCenter $ hLimit 100 $ hBox
    [ drawInstruction "HJ/arrows" "navigate"
    , drawInstruction "Enter"     "connect/disconnect"
    , drawInstruction "Esc/Q"     "exit"
    ]


drawBranchList :: L.List () Connection -> Widget ()
drawBranchList list =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel title
    $ hLimit 80
    $ L.renderList drawListElement True list
  where title = withAttr "title" $ str "Connections"


drawListElement :: Bool -> Connection -> Widget ()
drawListElement _ connection = padLeft (T.Pad 1) $ padRight T.Max $ attr $ str
  content
 where
  content = "[" ++ _cType connection ++ "] " ++ _name connection
  attr | _active connection = withAttr "current"
       | otherwise          = id

drawInstruction :: String -> String -> Widget n
drawInstruction keys action =
  C.hCenter $ withAttr "key" (str keys) <+> str " to " <+> withAttr
    "bold"
    (str action)


appHandleEvent :: State -> T.BrickEvent () e -> T.EventM () (T.Next State)
appHandleEvent state (T.VtyEvent e) =
  let checkoutBranch  = M.halt state
      deleteSelection = L.listClear state
      quit            = M.halt deleteSelection
  in  case e of
        V.EvKey V.KEsc        [] -> quit
        V.EvKey (V.KChar 'q') [] -> quit
        V.EvKey V.KEnter      [] -> checkoutBranch
        ev -> M.continue =<< L.handleListEventVi L.handleListEvent ev state

appHandleEvent state _ = M.continue state


initialState :: [Connection] -> State
initialState connections = L.list () (Vec.fromList connections) 1


selectedConnection :: State -> Maybe Connection
selectedConnection state = snd <$> L.listSelectedElement state

