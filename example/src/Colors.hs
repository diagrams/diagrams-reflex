{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Diagrams.Prelude as D
import Diagrams.Backend.Reflex
import Language.Javascript.JSaddle.Warp (run)
import Reflex
import Reflex.Dom.Core

main :: IO ()
main = run 3000 (mainWidget app)

app :: MonadWidget t m => m ()
app = mdo
  let svgDyn = fmap (reflexDia $ def & sizeSpec .~ mkWidth 400) diaDyn
  clicks <- switchHoldPromptly never <$> fmap diaMousedownEv =<< dyn svgDyn
  counts <- foldDyn countC mempty clicks
  let diaDyn = fmap mkDia counts
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
  Blank <> c = c
  c <> _ = c

instance Monoid C where
  mempty = Blank

instance Semigroup Counts where
  (Counts a b c) <> (Counts d e f) = Counts (a+d) (b+e) (c+f)

instance Monoid Counts where
  mempty = Counts 0 0 0

countC :: C -> Counts -> Counts
countC  Red (Counts r g b) = Counts (r+1) g b
countC  Green (Counts r g b) = Counts r (g+1) b
countC  Blue (Counts r g b)= Counts r g (b+1)
countC Blank cs = cs

annotate :: Monoid c => c -> Any -> c
annotate c a | getAny a = c
annotate _ _ = mempty
