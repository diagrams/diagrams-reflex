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
  ( Reflex(..) -- rendering token
  , B
    -- for rendering options specific to Reflex
  , Options(..), sizeSpec, svgAttributes
  -- ,svgDefinitions, idPrefix, svgAttributes, generateDoctype

  ) where

#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable            as F (foldMap)
#endif
import qualified Data.Text                as T
import           Data.Text.Lazy.IO        as LT
import           Data.Tree
import           System.FilePath

-- from base
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Typeable

-- from hashable
import           Data.Hashable            (Hashable (), hashWithSalt)

-- from bytestring
import qualified Data.ByteString          as SBS
import qualified Data.ByteString.Lazy     as BS

-- from lens
import           Control.Lens             hiding (transform, ( # ))

-- from diagrams-core
import           Diagrams.Core.Compile
import           Diagrams.Core.Types      (Annotation (..))

-- from diagrams-lib
import           Diagrams.Prelude         hiding (Attribute, size, view, local)
import           Diagrams.TwoD.Adjust     (adjustDia2D)
import           Diagrams.TwoD.Attributes (splitTextureFills)
import           Diagrams.TwoD.Path       (Clip (Clip))
import           Diagrams.TwoD.Text

-- from containers
import Data.Map (Map, singleton, fromList)

-- from reflex-dom
import Reflex.Dom.Class

-- from reflex-dom-contrib
import Reflex.Dom.Contrib.Widgets.Svg

-- from this package
import           Graphics.Rendering.Reflex   (Element(..), RenderM)
import qualified Graphics.Rendering.Reflex   as R

-- | @Reflex@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data Reflex = Reflex
  deriving (Show)

type B = Reflex

type instance V Reflex = V2
type instance N Reflex = Double

instance Monoid (Render Reflex V2 Double) where
  mempty = Render mempty
  Render r1 `mappend` Render r2_ = Render $ mappend r1 r2_

instance Backend Reflex V2 Double where
  newtype Render  Reflex V2 Double = Render RenderM
  type    Result  Reflex V2 Double = Element
  data    Options Reflex V2 Double = ReflexOptions
    { _size            :: SizeSpec V2 Double   -- ^ The requested size.
    , _svgAttributes   :: R.Attrs
                          -- ^ Attributes to apply to the entire svg element.
    }

  renderRTree :: Reflex -> Options Reflex V2 Double -> RTree Reflex V2 Double Annotation -> Result Reflex V2 Double
  renderRTree _ opts rt = Element "svg" attrs $ runReader (rtree rt) mempty
    where
      rtree :: RTree Reflex V2 Double Annotation -> RenderM
      rtree (Node n rs) = case n of
        RPrim p                 -> unRender $ render Reflex p
        RStyle sty              -> local (<> sty) r
        _                       -> r
        where
          r = foldMap rtree rs
      V2 w h = specToSize 100 . view sizeSpec $ opts
      attrs = fromList [ ("width", show w)
                       , ("height", show h) ]
              <> _svgAttributes opts

  adjustDia c opts d = adjustDia2D sizeSpec c opts (d # reflectY)

-- | Lens onto the size of the options.
sizeSpec :: Lens' (Options Reflex V2 Double) (SizeSpec V2 Double)
sizeSpec f opts = f (_size opts) <&> \s -> opts { _size = s }

-- | Lens onto the svgAttributes field of the options. This field
--   is provided to supply SVG attributes to the entire diagram.
svgAttributes :: Lens' (Options Reflex V2 Double) R.Attrs
svgAttributes f opts =
  f (_svgAttributes opts) <&> \ds -> opts { _svgAttributes = ds }

mkWidget :: forall t m. MonadWidget t m => Element -> m ()
mkWidget (Element el attrs children) = svgAttr el attrs (mapM_ mkWidget children)

unRender :: Render Reflex V2 Double -> RenderM
unRender (Render els) = els

instance Renderable (Path V2 Double) Reflex where
  render _ = Render . R.renderPath

instance Default (Options Reflex V2 Double) where
  def = ReflexOptions absolute mempty

Data DiaEv t a = DiaEv
  { -- diaMouseclickEv :: Event t a
  , diaMousedownEv :: Event t a
  , diaMouseupEv :: Event t a
  }


reflexDia :: forall t m a. (Monoid' a, MonadWidget t m) =>
             Options Reflex V2 Double -> QDiagram Reflex V2 Double a -> m (DiaEv t a)
reflexDia opts dia = do
  (clicks, _) <- svgAttr' n as $ mapM_ mkWidget cs
   md <- domEvent clicks Mousedown
   me <- domEvent clicks Mouseup
   return $ DiaEv (annotate md) (annotate mu)
    where
     (t, (Element n as cs)) = renderDiaT Reflex opts dia
     annotate = runQuery (query dia) . transform (inverse t) . v2

  -- instance (Hashable n, SVGFloat n) => Hashable (Options Reflex V2 Double) where
--   hashWithSalt s  (ReflexOptions sz sa) =
--     s  `hashWithSalt`
--     sz `hashWithSalt`
--     ds `hashWithSalt`
--     ia `hashWithSalt`
--     sa `hashWithSalt`
--     gd
--       where ds = fmap renderBS defs
