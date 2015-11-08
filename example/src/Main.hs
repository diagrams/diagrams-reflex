{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Diagrams.Prelude
import Diagrams.Backend.Reflex

main :: IO ()
main = reflexDia def (circle 1 # fc red)
