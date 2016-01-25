{-# LANGUAGE RecursiveDo #-}

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
app = mdo
  -- pos :: Event (V2 Double)
  pos <- switchPromptly never <$> fmap diaMousemovePos =<< dyn =<< mapDyn (reflexDia def) diaDyn
  -- diaDyn :: Dynamic (Diagram B)
  diaDyn <- holdDyn mempty (mkDia <$> pos)
  return ()

mkDia :: P2 Double -> Diagram B
mkDia p = c <> square 100 where
  c = moveTo p (circle 5) # fc green

-- fromLast :: a -> Option (Last a) -> a
-- fromLast a = option a getLast

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
