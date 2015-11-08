module Main where

import Diagrams.Prelude
import Diagrams.Backend.Reflex
import Reflex.Dom.Contrib.Widgets.Svg
import Reflex
import Reflex.Dom

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Char
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventM (preventDefault, eventTarget)
import           GHCJS.DOM.HTMLDocument
import           GHCJS.DOM.HTMLElement
import           Reflex.Dom hiding (getKeyEvent)
import           Reflex.Dom.Contrib.KeyEvent

main :: IO ()
main = appMain app

app :: MonadWidget t m => m ()
app = do
  ev <- reflexDia def (circle 100 :: Diagram B)
  ct <- count $ diaMousedownEv ev
  dynText =<< mapDyn counter ct
  return ()

counter :: Int -> String
counter i = "The circle has been clicked " ++ show i ++ " times"

------------------------------------------------------------------------------
waitUntilJust :: IO (Maybe a) -> IO a
waitUntilJust a = do
    mx <- a
    case mx of
      Just x -> return x
      Nothing -> do
        threadDelay 10000
        waitUntilJust a

------------------------------------------------------------------------------
appMain app = runWebGUI $ \webView -> do
    doc <- waitUntilJust $ fmap (fmap castToHTMLDocument) $
      webViewGetDomDocument webView
    body <- waitUntilJust $ fmap (fmap castToHTMLElement) $ documentGetBody doc
    attachWidget body webView app
