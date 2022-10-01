{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Diagrams.Backend.Reflex.FFI (elementLocalTopLeftById) where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle
  ( FromJSVal (fromJSVal),
    JSM,
    MonadJSM,
    ToJSVal (toJSVal),
    call,
    eval,
    liftJSM,
  )
import Reflex (TriggerEvent (..))
import Reflex.Dom.Core
  ( PerformEvent (Performable, performEvent),
    PostBuild (..),
    Reflex (Event),
    ffor,
  )
import Reflex.Time (tickLossyFromPostBuildTime)

elementLocalTopLeftById' :: Text -> JSM (Maybe (Double, Double))
elementLocalTopLeftById' name = do
  name' <- toJSVal name
  f <-
    eval $
      T.pack
        "(function(name){ \
        \const ele = document.getElementById(name); \
        \var rect = ele.getBoundingClientRect();\
        \var x = rect.left; \
        \var y = rect.top; \
        \return [x,y];\
        \})"
  r <- call f f [name']
  fromJSVal r

liftJSM_ ::
  (PerformEvent t m, MonadJSM (Performable m)) =>
  Event t () ->
  JSM a ->
  m (Event t a)
liftJSM_ n f = performEvent $ ffor n $ \_ -> liftJSM f

-- | Poll every 100ms to obtain the top-left coordinate of the given element.
elementLocalTopLeftById ::
  ( PostBuild t m,
    PerformEvent t m,
    MonadJSM (Performable m),
    TriggerEvent t m,
    MonadFix m
  ) =>
  Text ->
  m (Event t (Maybe (Double, Double)))
elementLocalTopLeftById name = do
  t <- tickLossyFromPostBuildTime 0.1
  liftJSM_ (void t) (elementLocalTopLeftById' name)
