{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Diagrams.Prelude
import           Diagrams.Backend.Reflex
import           Reflex
import           Reflex.Dom
import           Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = mainWidget app

app :: MonadWidget t m => m ()
app = do
  ev <- reflexDia def (circle 100 # lc blue # lwL 2)
  ct <- count . ffilter getAny $ diaMousedownEv ev
  dynText $ fmap counter ct
  return ()

counter :: Int -> Text
counter i = T.concat ["The circle has been clicked ", T.pack (show i), " times" ]
