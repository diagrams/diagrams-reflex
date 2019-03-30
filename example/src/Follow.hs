{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
  let svgDyn = fmap (reflexDia $ def & sizeSpec .~ dims2D 500 1000) diaDyn
  pos <- switchHoldPromptly never <$> fmap diaMousemovePos =<< dyn svgDyn
  diaDyn <- holdDyn (mkDia . p2 $ (0, -1000)) (mkDia <$> pos)
  return ()

-- TODO generalize and move into diagrams-lib
constrain :: (InSpace v n a, Enveloped a, HasBasis v, Ord (v n)) =>
             a -> Point v n -> Point v n
constrain a p = maybe p c (getCorners $ boundingBox a)
  where
    c (l, h) = max l (min h p)

mkDia :: P2 Double -> Diagram B
mkDia p = arr <> c <> back where
  arr = arrowBetween'
    (def & arrowHead .~ dart & arrowTail .~ quill )
    origin p'
  c = moveTo p' $ D.text "Hello" # fc green
  p' = constrain back p
  back = vcat [ square 1000 # fc cyan, square 1000 # fc yellow ]
