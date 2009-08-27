{-# LANGUAGE BangPatterns #-}
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

module Graphics.Rendering.Hieroglyph.OpenGL.Render where

import qualified Graphics.UI.GLUT as GLUT
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
import qualified App.Widgets.MouseKeyboard as Buster
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import qualified Text.PrettyPrint as Pretty
import System.Mem.Weak

import Graphics.Rendering.Hieroglyph.OpenGL.Compile
import Graphics.Rendering.Hieroglyph.OpenGL.Data

renderCompiledGeometry z (CompiledDots prim vdata iid) = do
    let verticesFrom (x:y:vs) = GL.Vertex3 x y z : verticesFrom vs
        verticesFrom [] = []
    loadAttrs . attribs $ prim
    GL.pointSize $= (realToFrac . alinewidth . attribs $ prim)
    GL.color . colourToGL . afillRGBA . attribs $ prim
    GL.withName (GL.Name iid)  . GL.renderPrimitive GL.Points . mapM_ GL.vertex . verticesFrom $ vdata

renderCompiledGeometry z obj@(CompiledArc _ _ _) =
    (GL.textureBinding GL.Texture2D $= Nothing) >> renderObject z obj

renderCompiledGeometry z obj@(CompiledPath _ _ _) =
    (GL.textureBinding GL.Texture2D $= Nothing) >> renderObject z obj

renderCompiledGeometry z obj@(CompiledRectangle prim x y w h iid) = do
    GL.textureBinding GL.Texture2D $= Nothing
    loadAttrs attrs

    GL.lineSmooth $= GL.Disabled
    GL.polygonSmooth $= GL.Disabled
    GL.color . colourToGL . afillRGBA $ attrs

    when (afilled attrs) .
        GL.withName (GL.Name iid) .
        GL.renderPrimitive GL.Quads .
        mapM_ GL.vertex
        $ take 4 vertices

    GL.color . colourToGL . astrokeRGBA $ attrs
    GL.lineSmooth $= GL.Enabled
    GL.polygonSmooth $= GL.Enabled

    when (aoutlined attrs) .
        GL.withName (GL.Name iid) .
        GL.renderPrimitive GL.LineStrip .
        mapM_ GL.vertex
        $ vertices

    where attrs = attribs prim
          vertices = [GL.Vertex3 x     y        z
                     ,GL.Vertex3 (x+w) y        z
                     ,GL.Vertex3 (x+w) (y+h)    z
                     ,GL.Vertex3 x      (y+h)   z
                     ,GL.Vertex3 x      y       z]



renderCompiledGeometry z obj@(CompiledImage original x y w h tex iid) = do
    GL.textureFunction $= GL.Replace
    GL.color $ GL.Color4 1 1 1 (1::Double)
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just tex
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureFunction $= GL.Replace

    GL.lineSmooth $= GL.Disabled
    GL.polygonSmooth $= GL.Disabled



    GL.withName (GL.Name iid)  . GL.renderPrimitive GL.Quads $ do
        GL.texCoord $ GL.TexCoord2 0 (1::Double)
        GL.vertex   $ GL.Vertex3 x y z
        GL.texCoord $ GL.TexCoord2 1 (1::Double)
        GL.vertex   $ GL.Vertex3 (x+w) y z
        GL.texCoord $ GL.TexCoord2 1 (0::Double)
        GL.vertex   $ GL.Vertex3 (x+w) (y+h) z
        GL.texCoord $ GL.TexCoord2 0 (0::Double)
        GL.vertex   $ GL.Vertex3 x (y+h) z
        GL.flush

    GL.lineSmooth $= GL.Enabled
    GL.polygonSmooth $= GL.Enabled

renderObject z obj
    | afilled (attribs . original $ obj) = GL.preservingMatrix $ do

        GL.textureBinding GL.Texture2D $= Nothing
        loadAttrs (attribs . original $  obj )
        GL.color . colourToGL . afillRGBA . attribs . original $ obj
        GL.withName (GL.Name (uid obj)) . GL.renderPrimitive GL.TriangleFan . mapM_ GL.vertex . verticesFrom $ vertices obj
        GL.color . colourToGL . astrokeRGBA . attribs . original $ obj
        when (aoutlined (attribs . original $  obj)) $ (GL.withName (GL.Name (uid obj)) . GL.renderPrimitive GL.LineStrip . mapM_ GL.vertex . verticesFrom . drop 2 $ vertices obj)
    | otherwise                = GL.preservingMatrix $ do

        loadAttrs (attribs . original $  obj)
        GL.color . colourToGL . astrokeRGBA . attribs . original $ obj
        GL.withName (GL.Name (uid obj)) . GL.renderPrimitive GL.LineStrip . mapM_ GL.vertex . verticesFrom . drop 2 $ vertices obj

    where verticesFrom (x:y:vs) = GL.Vertex3 x y z : verticesFrom vs
          verticesFrom [] = []



loadAttrs attrs = GL.preservingMatrix $ do
    return ()
    -- TODO support line cap
    -- TODO support line join
    -- TODO support miter limit
    -- TODO support trapezoidal tolerance
    -- TODO support operator
    -- TODO support antialias
    -- TODO support dash/stipple
    -- TODO support pattern fill rule


getGeo (Geometry x) =  x
getGeo _ = []

render runtime@HgGLUT{} geo = do
    GL.drawBuffer $= GL.BackBuffers
    (GL.Position px py, GL.Size sx sy ) <- GL.get GL.viewport
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    maybe (GL.ortho 0 (fromIntegral sx) 0 (fromIntegral sy) 1 2)
          (\(a,b,c,d) -> GL.ortho a b c d 1 2)
          (ortho runtime)
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.depthFunc $= Just GL.Less
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.lineSmooth $= GL.Enabled
    GL.polygonSmooth $= GL.Enabled
    GL.pointSmooth $= GL.Enabled
    r' <- renderObjects Nothing [1::Double,2..] (sort geo) runtime
    GLUT.swapBuffers
    return r'{ texture_blacklist = [] }
render runtime@HgGL{} geo = Gtk.withGLDrawingArea (drawarea runtime) $ \drawable -> do
    ctx <- Gtk.glDrawingAreaGetGLContext ( drawarea runtime )
    Gtk.glDrawableGLBegin drawable ctx
    GL.drawBuffer $= GL.BackBuffers
    (GL.Position px py, GL.Size sx sy ) <- GL.get GL.viewport
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    maybe (GL.ortho 0 (fromIntegral sx) 0 (fromIntegral sy) 1 2)
          (\(a,b,c,d) -> GL.ortho a b c d 1 2)
          (ortho runtime)
    GL.clearColor $= GL.Color4 1 1 1 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.depthFunc $= Just GL.Less
    GL.blend $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.lineSmooth $= GL.Enabled
    GL.polygonSmooth $= GL.Enabled
    GL.pointSmooth $= GL.Enabled
    r' <- renderObjects Nothing [1::Double,2..] (sort geo) runtime
    Gtk.glDrawableSwapBuffers drawable
    Gtk.glDrawableGLEnd drawable
    return r'{ texture_blacklist = [] }


renderObjects isSelection (z:zs) (o:os) !r = renderObj isSelection ((-2) + z/(z+1)) o r >>= renderObjects isSelection zs os
renderObjects _ _ [] r = return r

renderObj :: Maybe (Double,Double) -> Double -> Primitive -> HieroglyphGLRuntime -> IO HieroglyphGLRuntime
renderObj isSelection z obj runtime = do
        (runtime',cg0) <- compile runtime obj
        let attrs = attribs obj
        GL.lineWidth $= (realToFrac . alinewidth $ attrs)
        GL.matrixMode $= GL.Modelview 0
        GL.loadIdentity
        GL.translate $ GL.Vector3 (atranslatex attrs) (atranslatey attrs) 0
        GL.scale (ascalex attrs) (ascaley attrs) 1
        GL.rotate (arotation attrs) $ GL.Vector3 0 0 1
        GL.lineSmooth $= GL.Enabled
        GL.polygonSmooth $= GL.Enabled

        when ((isJust (ortho runtime)) && (astatic (attribs obj))) $ do
            GL.matrixMode $= GL.Projection
            GL.loadIdentity
            (p,GL.Size rx ry) <- GL.get GL.viewport
            maybe (return ()) (\(selx,sely) -> GL.pickMatrix (selx-2, (fromIntegral ry)-sely+2) (6,6) (p, GL.Size rx ry)) isSelection
            GL.ortho 0 (fromIntegral rx) 0 (fromIntegral ry) 1 2

        renderCompiledGeometry z cg0

        when ((isJust (ortho runtime)) && (astatic (attribs obj))) $ do
            GL.matrixMode $= GL.Projection
            GL.loadIdentity
            let (a,b,c,d) = fromJust (ortho runtime) in GL.ortho a b c d 1 2
            GL.matrixMode $= GL.Modelview 0

        return runtime'{ namemap=Map.insert (uid cg0)
                                            (fromMaybe "" . aname . attribs . original $ cg0)
                                            (namemap runtime') }

