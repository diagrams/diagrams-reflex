{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RankNTypes #-}
  -- UndecidableInstances needed for ghc < 707

----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Reflex
-- Copyright   :  (c) 2015 diagrams-svg team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Reflex
  ( ReflexSvg(..) -- rendering token
  , B
    -- for rendering options specific to Reflex
  , Options(..), sizeSpec, svgAttributes
  -- ,svgDefinitions, idPrefix, svgAttributes, generateDoctype
  , DiaEv(..)
  , reflexDia
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable            as F (foldMap)
#endif
import           Data.Tree

-- from base
import           Control.Monad.Reader

-- from lens
import           Control.Lens             hiding (children, transform, ( # ))

-- from diagrams-core
import           Diagrams.Core.Compile
import           Diagrams.Core.Types      (Annotation (..))

-- from diagrams-lib
import           Diagrams.Prelude         hiding (Attribute, size, view, local, text)
import           Diagrams.TwoD.Adjust     (adjustDia2D)
import qualified Diagrams.TwoD.Text       as D2T

-- from containers
import qualified Data.Map as M

-- from reflex
import           Reflex

-- from reflex-dom
import           Reflex.Dom.Builder.Class (EventName (..), EventResultType, domEvent)
import           Reflex.Dom.Old (MonadWidget)
import           Reflex.Dom.Widget.Basic

-- from reflex-dom-contrib
import           Reflex.Dom.Contrib.Widgets.Svg

-- from text
import           Data.Text (Text)
import qualified Data.Text as T

-- from this package
import           Graphics.Rendering.Reflex   (Element(..), RenderM)
import qualified Graphics.Rendering.Reflex   as R

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | @ReflexSvg@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data ReflexSvg = ReflexSvg
  deriving (Show)

type B = ReflexSvg

type instance V ReflexSvg = V2
type instance N ReflexSvg = Double

instance Monoid (Render ReflexSvg V2 Double) where
  mempty = Render mempty
  Render r1 `mappend` Render r2_ = Render $ mappend r1 r2_

instance Backend ReflexSvg V2 Double where
  newtype Render  ReflexSvg V2 Double = Render RenderM
  type    Result  ReflexSvg V2 Double = Element
  data    Options ReflexSvg V2 Double = ReflexOptions
    { _size            :: SizeSpec V2 Double   -- ^ The requested size.
    , _svgAttributes   :: R.Attrs
                          -- ^ Attributes to apply to the entire svg element.
    }

  renderRTree :: ReflexSvg -> Options ReflexSvg V2 Double -> RTree ReflexSvg V2 Double Annotation -> Result ReflexSvg V2 Double
  renderRTree _ opts rt = Element "svg" attrs $ runReader (rtree rt) mempty
    where
      rtree :: RTree ReflexSvg V2 Double Annotation -> RenderM
      rtree (Node n rs) = case n of
        RPrim p                 -> unRender $ render ReflexSvg p
        RStyle sty              -> local (<> sty) r
        _                       -> r
        where
          r = foldMap rtree rs
      V2 w h = specToSize 100 . view sizeSpec $ opts
      attrs = M.fromList [ ("width", tshow w)
                       , ("height", tshow h) ]
              <> _svgAttributes opts

  adjustDia c opts d = ( sz, t <> reflectionY, d' ) where
    (sz, t, d') = adjustDia2D sizeSpec c opts (d # reflectY)

-- | Lens onto the size of the options.
sizeSpec :: Lens' (Options ReflexSvg V2 Double) (SizeSpec V2 Double)
sizeSpec f opts = f (_size opts) <&> \s -> opts { _size = s }

-- | Lens onto the svgAttributes field of the options. This field
--   is provided to supply SVG attributes to the entire diagram.
svgAttributes :: Lens' (Options ReflexSvg V2 Double) R.Attrs
svgAttributes f opts =
  f (_svgAttributes opts) <&> \ds -> opts { _svgAttributes = ds }

mkWidget :: forall t m. MonadWidget t m => Element -> m ()
mkWidget (Element name attrs children) = svgAttr name attrs (mapM_ mkWidget children)
mkWidget (SvgText str) = text str

unRender :: Render ReflexSvg V2 Double -> RenderM
unRender (Render els) = els

instance Renderable (Path V2 Double) ReflexSvg where
  render _ = Render . R.renderPath

instance Renderable (D2T.Text Double) ReflexSvg where
  render _ = Render . R.renderText

instance Default (Options ReflexSvg V2 Double) where
  def = ReflexOptions absolute mempty

data DiaEv t a = DiaEv
                 { diaMousedownEv :: Event t a
                 , diaMouseupEv :: Event t a
                 , diaMousemoveEv :: Event t a
                 , diaMousedownPos :: Event t (P2 Double)
                 , diaMouseupPos :: Event t (P2 Double)
                 , diaMousemovePos :: Event t (P2 Double)
                 }

reflexDia :: forall t m a. (Monoid' a, MonadWidget t m) =>
             Options ReflexSvg V2 Double -> QDiagram ReflexSvg V2 Double a -> m (DiaEv t a)
reflexDia opts dia = do
  -- render SVG, get stream with all events
  let (t, (Element n as cs)) = renderDiaT ReflexSvg opts dia
  (allEvents, _) <- svgAttr' n as $ mapM_ mkWidget cs
  -- particular event streams
  let
    q :: forall en. EventResultType en ~ (Int, Int) => EventName en -> Event t a
    q eventType = Diagrams.Prelude.sample dia <$> pos eventType
    pos :: forall en. EventResultType en ~ (Int, Int) => EventName en -> Event t (P2 Double)
    pos en = transform (inv t) . fmap fromIntegral . p2 <$> domEvent en allEvents
  return $ DiaEv
    (q Mousedown)
    (q Mouseup)
    (q Mousemove)
    (pos Mousedown)
    (pos Mouseup)
    (pos Mousemove)
