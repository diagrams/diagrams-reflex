module Main where

import Diagrams.Prelude
import Diagrams.Backend.Reflex
import Reflex
import Reflex.Dom

main :: IO ()
main = mainWidget app

app :: MonadWidget t m => m ()
app = do
  ev <- reflexDia def (circle 100 # lc blue # lwL 2)
  ct <- count . ffilter getAny $ diaMousedownEv ev
  dynText =<< mapDyn counter ct
  return ()

counter :: Int -> String
counter i = "The circle has been clicked " ++ show i ++ " times"
