{-# LANGUAGE RecursiveDo #-}

module Main where

import Diagrams.Prelude as D
import Diagrams.Backend.Reflex
import Reflex
import Reflex.Dom

import           Control.Concurrent
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.HTMLDocument
import           GHCJS.DOM.HTMLElement

main :: IO ()
main = mainWidget app

app :: MonadWidget t m => m ()
app = mdo
  -- svgDyn :: Dynamic t (m (DiaEv C)
  svgDyn <- mapDyn (reflexDia $ def & sizeSpec .~ mkWidth 400) diaDyn
  -- clicks :: Event t C
  clicks <- switchPromptly never <$> fmap diaMousedownEv =<< dyn svgDyn
  counts <- foldDyn countC mempty clicks
  -- diaDyn <- holdDyn (mkDia mempty) (mkDia <$> counts)
  diaDyn <- mapDyn mkDia counts
  return ()

mkDia :: Counts -> QDiagram B V2 Double C
mkDia (Counts r g b) = hcat
  [ annotate Red <$> textD r <> fc red unitCircle
  , annotate Green <$> textD g <> fc green unitCircle
  , annotate Blue <$> textD b <> fc blue unitCircle
  ]

textD :: (Show a, Monoid m) => a -> QDiagram B V2 Double m
textD = fmap (annotate mempty) . D.text . show

data C = Red | Green | Blue | Blank

data Counts = Counts Int Int Int

instance Semigroup C where
  (<>) = mappend

instance Monoid C where
  mempty = Blank
  mappend Blank c = c
  mappend c _ = c

instance Monoid Counts where
  mempty = Counts 0 0 0
  mappend (Counts a b c) (Counts d e f) = Counts (a+d) (b+e) (c+f)

countC :: C -> Counts -> Counts
countC  Red (Counts r g b) = Counts (r+1) g b
countC  Green (Counts r g b) = Counts r (g+1) b
countC  Blue (Counts r g b)= Counts r g (b+1)
countC Blank cs = cs

annotate :: Monoid c => c -> Any -> c
annotate c a | getAny a = c
annotate _ _ = mempty
