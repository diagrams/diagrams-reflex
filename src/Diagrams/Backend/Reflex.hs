{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Reflex
-- Copyright   :  (c) 2015 diagrams-svg team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Reflex
  ( ReflexSvg(..)
  , B
  , Options(..)
  , sizeSpec
  , svgAttributes
  , DiaEv(..)
  , reflexDia
  ) where

import Control.Monad (void)
import Control.Monad.Reader (runReader, local)
import Control.Lens (view)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Diagrams.Backend.Reflex.FFI (elementLocalTopLeftById)
import Diagrams.Prelude hiding (Attribute, local, size, text, view)
import Diagrams.TwoD.Adjust (adjustDia2D)
import qualified Diagrams.TwoD.Text as D2T
import Reflex
import Reflex.Dom.Class ((=:))
import Reflex.Dom.Old (MonadWidget)
import Reflex.Dom.Widget.Basic
import Graphics.Rendering.Reflex (Element(..), RenderM)
import qualified Graphics.Rendering.Reflex as R

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | @ReflexSvg@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data ReflexSvg = ReflexSvg
  deriving (Show)

type B = ReflexSvg

type instance V ReflexSvg = V2
type instance N ReflexSvg = Double

instance Semigroup (Render ReflexSvg V2 Double) where
  Render r1 <> Render r2_ = Render $ r1 <> r2_

instance Monoid (Render ReflexSvg V2 Double) where
  mempty = Render mempty

instance Backend ReflexSvg V2 Double where
  newtype Render ReflexSvg V2 Double = Render RenderM
  type Result ReflexSvg V2 Double = Element
  data Options ReflexSvg V2 Double = ReflexOptions
    { -- | The requested size.
      _size :: SizeSpec V2 Double,
      -- | Attributes to apply to the entire svg element.
      _svgAttributes :: R.Attrs,
      -- | The id of the enclosing svg
      _elementId :: Text
    }

  renderRTree ::
       ReflexSvg
    -> Options ReflexSvg V2 Double
    -> RTree ReflexSvg V2 Double Annotation
    -> Result ReflexSvg V2 Double
  renderRTree _ opts rt = Element "svg" attrs $ runReader (rtree rt) mempty
    where
      rtree :: RTree ReflexSvg V2 Double Annotation -> RenderM
      rtree (Node n rs) = case n of
        RPrim p                 -> unRender $ render ReflexSvg p
        RStyle sty              -> local (<> sty) (foldMap rtree rs)
        _                       -> foldMap rtree rs
      V2 w h = specToSize 100 . view sizeSpec $ opts
      attrs = M.fromList [("width", tshow w), ("height", tshow h)]
              <> _svgAttributes opts

  adjustDia c opts d = ( sz, t <> reflectionY, d' ) where
    (sz, t, d') = adjustDia2D sizeSpec c opts (d # reflectY)

-- | Lens onto the size of the options.
sizeSpec :: Lens' (Options ReflexSvg V2 Double) (SizeSpec V2 Double)
sizeSpec f opts = f (_size opts) <&> \s -> opts {_size = s}

-- | Lens onto the svgAttributes field of the options. This field
--   is provided to supply SVG attributes to the entire diagram.
svgAttributes :: Lens' (Options ReflexSvg V2 Double) R.Attrs
svgAttributes f opts =
  f (_svgAttributes opts) <&> \ds -> opts {_svgAttributes = ds}

mkWidget :: forall t m. MonadWidget t m => Element -> m ()
mkWidget (Element name attrs children) =
  void $ elDynAttrNS' (Just svgNS) name (pure attrs) (mapM_ mkWidget children)
mkWidget (SvgText str) = text str

unRender :: Render ReflexSvg V2 Double -> RenderM
unRender (Render els) = els

instance Renderable (Path V2 Double) ReflexSvg where
  render _ = Render . R.renderPath

instance Renderable (D2T.Text Double) ReflexSvg where
  render _ = Render . R.renderText

instance Default (Options ReflexSvg V2 Double) where
  def = ReflexOptions absolute mempty "diagram"

data DiaEv t a = DiaEv
  { diaMousedownEv :: Event t a
  , diaMouseupEv :: Event t a
  , diaMousemoveEv :: Event t a
  , diaMousedownPos :: Event t (P2 Double)
  , diaMouseupPos :: Event t (P2 Double)
  , diaMousemovePos :: Event t (P2 Double)
  }

reflexDia ::
  forall t m a.
  (Monoid' a, MonadWidget t m) =>
  Options ReflexSvg V2 Double ->
  QDiagram ReflexSvg V2 Double a ->
  m (DiaEv t a)
reflexDia opts dia = do
  -- render SVG, get stream with all events
  let (t, Element n as cs) = renderDiaT ReflexSvg opts dia
      eleId = _elementId opts
  (allEvents, _) <-
    elDynAttrNS'
      (Just svgNS)
      n
      (pure (as <> ("id" =: eleId)))
      $ mapM_ mkWidget cs
  relPosE <- elementLocalTopLeftById eleId
  -- particular event streams
  let q :: forall en. EventResultType en ~ (Int, Int) => EventName en -> m (Event t a)
      q eventType = fmap (Diagrams.Prelude.sample dia) <$> pos eventType
      pos ::
        forall en.
        EventResultType en ~ (Int, Int) =>
        EventName en ->
        m (Event t (P2 Double))
      pos en = switchHold never $
        ffor relPosE $
          \case
            Nothing -> transform (inv t) <$> globalCords
            Just localCoords ->
              let localCoords' = p2 localCoords
               in transform (inv t) . (\g -> g - localCoords') <$> globalCords
        where
          globalCords =
            fmap fromIntegral . p2
                <$> domEvent en allEvents

  DiaEv
    <$> q Mousedown
    <*> q Mouseup
    <*> q Mousemove
    <*> pos Mousedown
    <*> pos Mouseup
    <*> pos Mousemove

svgNS :: Text
svgNS = "http://www.w3.org/2000/svg"
