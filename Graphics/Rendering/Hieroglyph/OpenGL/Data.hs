{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.Hieroglyph.OpenGL
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  J.R. Heard
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Hieroglyph.OpenGL.Data where

import qualified Graphics.Rendering.Hieroglyph.Cache as Cache
import System.Exit
import GHC.Float
import Data.List
import Control.Concurrent
import Control.Applicative
import Control.Monad.Trans
import qualified System.Glib.MainLoop as Gtk
import Data.List (partition)
import qualified Data.Set as Set
import Data.Maybe
import Graphics.UI.Gtk.Cairo as Cairo
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Data.Array.MArray as A
import Control.Monad
import Graphics.UI.Gtk.Pango.Context
import Graphics.UI.Gtk.Pango.Layout
import Foreign
import qualified Data.Map as Map
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.OpenGL as Gtk
import qualified Graphics.UI.Gtk.OpenGL.Drawable as Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Gtk
import qualified Data.ByteString.Internal as SB
import qualified Graphics.Rendering.Cairo as Cairo -- for rendering fonts
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL(GLuint, Vertex2, ($=))
import Graphics.Rendering.Hieroglyph.Primitives
import Graphics.Rendering.Hieroglyph.Visual
import qualified Data.ByteString as SB
import Foreign.C
import qualified App.EventBus as Buster
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import qualified Text.PrettyPrint as Pretty
import System.Mem.Weak

data HieroglyphGLRuntime = HgGL {
    texture_whitelist :: [GL.TextureObject]
  , texture_greylist :: Cache.Cache String GL.TextureObject
  , texture_blacklist :: [GL.TextureObject]

  , buffer_whitelist :: [GL.BufferObject]
  , buffer_greylist :: Cache.Cache Int GL.BufferObject
  , buffer_blacklist :: [GL.BufferObject]

  , namemap :: Map.Map GLuint String
  , drawarea :: Gtk.GLDrawingArea
  , window :: Gtk.Window
  , context ::PangoContext
  , texdims :: Map.Map GL.TextureObject (Double,Double)

  , ortho :: Maybe (Double,Double,Double,Double)
  }
  | HgGLUT {
    texture_whitelist :: [GL.TextureObject]
  , texture_greylist :: Cache.Cache String GL.TextureObject
  , texture_blacklist :: [GL.TextureObject]

  , buffer_whitelist :: [GL.BufferObject]
  , buffer_greylist :: Cache.Cache Int GL.BufferObject
  , buffer_blacklist :: [GL.BufferObject]

  , namemap :: Map.Map GLuint String
  , context ::PangoContext
  , texdims :: Map.Map GL.TextureObject (Double,Double)

  , ortho :: Maybe (Double,Double,Double,Double)
  }
  | Geometry BaseVisual
  | AttributedCoords Double Double [String]

class VisualEventData a where
    getHieroglyphData :: a -> HieroglyphGLRuntime
    setHieroglyphData :: HieroglyphGLRuntime -> a -> a
    newHieroglyphData :: HieroglyphGLRuntime -> a

instance VisualEventData [Buster.EData HieroglyphGLRuntime] where
    getHieroglyphData = Buster.fromEOther . head
    setHieroglyphData r e = Buster.EOther r : tail e
    newHieroglyphData r = [Buster.EOther r]

reverseMouseCoords b x y = do
    let renderDataE = fromJust $ Buster.eventByQName "Hieroglyph" "Hieroglyph" "RenderData" b
    (_,sy) <- Gtk.widgetGetSize . Gtk.castToWidget . drawarea . (\(Buster.EOther a) -> a) . head . Buster.eventdata $ renderDataE
    return (Point x (fromIntegral sy-y))

data CompiledData =
    CompiledDots
        { original :: Primitive
        , vertices :: [Double]
        , uid :: GLuint }
  | CompiledArc
        { original :: Primitive
        , vertices :: [Double]
        , uid :: GLuint }
  | CompiledPath
        { original :: Primitive
        , vertices :: [Double]
        , uid :: GLuint }
  | CompiledRectangle
        { original :: Primitive
        , xx :: Double
        , yy :: Double
        , ww :: Double
        , hh :: Double
        , uid :: GLuint }
  | CompiledImage
        { original :: Primitive
        , xx :: Double
        , yy :: Double
        , ww :: Double
        , hh :: Double
        , texture :: GL.TextureObject
        , uid :: GLuint }


texturedObjects (CompiledImage _ _ _  _ _ _  _) = True
texturedObjects _ = False

colourToTuple :: AlphaColour Double -> (Double,Double,Double,Double)
colourToTuple c = (r,g,b,alpha)
    where alpha = alphaChannel c
          c' = if alpha > 0 then (1/alpha) `darken` (c `Data.Colour.over` black) else black
          RGB r g b = toSRGB c'

colourToGL :: AlphaColour Double -> GL.Color4 Double
colourToGL = (\(r,g,b,a) -> GL.Color4 r g b a) . colourToTuple

cacheimg img = show (filename img, {- show $ dimensions img,-} preserveaspect img)
cachetxt txt = show (show . str $ txt,align txt,wrapwidth txt,wrapmode txt,justify txt,indent txt,spacing txt)

