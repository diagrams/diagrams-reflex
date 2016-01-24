module Main where

import Diagrams.Prelude
import Diagrams.Backend.Reflex
import Reflex
import Reflex.Dom

import           Control.Concurrent
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.HTMLDocument
import           GHCJS.DOM.HTMLElement

main :: IO ()
main = appMain app

app :: MonadWidget t m => m ()
app = do
  ev <- reflexDia def (circle 100)
  ct <- count . ffilter getAny $ diaMousedownEv ev
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
-- | Launch a Reflex app when the page loads
appMain reflexApp = runWebGUI $ \webView -> do
    doc <- waitUntilJust $ fmap (fmap castToHTMLDocument) $
      webViewGetDomDocument webView
    body <- waitUntilJust $ fmap (fmap castToHTMLElement) $ getBody doc
    attachWidget body webView reflexApp
