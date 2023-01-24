module Main where

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Main
import Brick.AttrMap
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input
import Control.Monad (void)

drawUi :: () -> [Widget ()]
drawUi () = let
  greedyText = Widget Greedy Fixed $ do
    render $ hCenter $ str "ok"
  mainW = border $ hLimit 15 $ hBox [greedyText, str "1234567890", greedyText, str "123456", greedyText]
  doc = str "Prsss Esc to quit"
  in [ vCenter $ vBox [ hCenter mainW, hCenter doc ] ]

handleEvent :: BrickEvent () () -> EventM () () ()
handleEvent e = case e of
  VtyEvent (EvKey KEsc _) -> halt
  _ -> return ()

main :: IO ()
main = do
  let app = App { appDraw = drawUi
                , appChooseCursor = neverShowCursor
                , appHandleEvent = handleEvent
                , appStartEvent = return ()
                , appAttrMap = const $ attrMap defAttr [] }
  void $ defaultMain app ()
