{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Diagrams.Prelude as D
import Diagrams.Backend.Reflex as DR
import Reflex as R
import Reflex.Dom as R

main :: IO ()
main = mainWidget app

app :: MonadWidget t m => m ()
app = mdo
  -- svgDyn :: Dynamic t (m (DiaEv Any))
  let svgDyn = fmap (reflexDia $ def & sizeSpec .~ dims2D 500 1000) diaDyn
  -- pos :: Event (V2 Double)
  pos <- switchPromptly never <$> fmap diaMousemovePos =<< dyn svgDyn
  -- diaDyn :: Dynamic (Diagram B)
  diaDyn <- holdDyn (mkDia . p2 $ (0, -1000)) (mkDia <$> pos)
  return ()

-- TODO generalize and move into diagrams-lib
constrain :: (InSpace v n a, Enveloped a, HasBasis v, Num n, Ord (v n)) =>
             a -> Point v n -> Point v n
constrain a p = maybe p c $ getCorners box where
  c (l,h) = max l (min h p)
  box = boundingBox a

mkDia :: P2 Double -> Diagram B
mkDia p = arr <> c <> back where
  arr = arrowBetween'
    (def & arrowHead .~ dart & arrowTail .~ quill )
    origin p'
  c = moveTo p' $ D.text "Hello" # fc green
  p' = constrain back p
  back = vcat [ square 1000 # fc cyan, square 1000 # fc yellow ]
