{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Diagrams.Prelude as D
import Diagrams.Backend.Reflex as DR
import Reflex as R
import Reflex.Dom as R

import           Control.Concurrent
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.HTMLDocument
import           GHCJS.DOM.HTMLElement

main :: IO ()
main = appMain app

app :: MonadWidget t m => m ()
app = mdo
  -- svgDyn :: Dynamic t (m (DiaEv Any))
  svgDyn <- mapDyn (reflexDia $ def & sizeSpec .~ dims2D 500 1000) diaDyn
  -- pos :: Event (V2 Double)
  pos <- switchPromptly never <$> fmap diaMousemovePos =<< dyn svgDyn
  -- diaDyn :: Dynamic (Diagram B)
  diaDyn <- holdDyn mempty (mkDia <$> pos)
  return ()

-- TODO generalize and move into diagrams-lib
constrain :: (InSpace V2 Double a, Enveloped a) =>
             a -> P2 Double -> P2 Double
constrain a p = maybe p c $ getCorners box where
  c (l,h) = max l (min h p)
  box = boundingBox a

mkDia :: P2 Double -> Diagram B
mkDia p = c <> bg where
  c = moveTo (constrain bg p) (D.text "Hello") # fc green
  bg = vcat [ square 1000 # fc cyan, square 1000 # fc yellow ]

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
