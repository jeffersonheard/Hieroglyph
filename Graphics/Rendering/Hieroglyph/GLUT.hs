{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.Hieroglyph.OpenGL
-- Copyright   :  Renassance Computing Institute 2009
-- License     :  BSD3
--
-- Maintainer  :  J.R. Heard
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.Rendering.Hieroglyph.GLUT
    ( module Graphics.Rendering.Hieroglyph.OpenGL.Data
    , mouseSelectionBehaviour
    , boilerplateOpenGLMain
    , renderOnExpose
    , renderBehaviour
    , selectionBehaviour
    , initializeBus)
where

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
import App.Widgets.MouseKeyboardGLUT
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import qualified Text.PrettyPrint as Pretty
import System.Mem.Weak

import Graphics.Rendering.Hieroglyph.OpenGL.Render
import Graphics.Rendering.Hieroglyph.OpenGL.Data
import Graphics.Rendering.Hieroglyph.OpenGL.Compile
import qualified Graphics.UI.GLUT as GLUT

-- | Select based on mouse clicks
mouseSelectionBehaviour :: VisualEventData a => Buster.Behaviour a
mouseSelectionBehaviour bus = Buster.pollFullyQualifiedEventWith bus "Mouse" "Hieroglyph.KeyboardMouseWidget" "SingleClick" $ \event -> do
    let (AttributedCoords x y _) = getHieroglyphData . Buster.eventdata $ event
    Buster.listM $ Buster.produce "Hieroglyph" "Hieroglyph" "PleaseSelect" Buster.once (newHieroglyphData $ AttributedCoords x y [])

boilerplateOpenGLMain :: VisualEventData a => [Buster.Widget a] -> Buster.Behaviour a -> IO ()
boilerplateOpenGLMain widgets behaviour = do
    evBus <- newMVar Buster.emptyBus
    forM_ widgets ($evBus)
    b <- takeMVar evBus
    putMVar evBus b

    let loop mv = do
            GLUT.mainLoopEvent
            Buster.busIteration mv behaviour
            loop mv

    let mk = bindMouseKeyboardWidget "Hieroglyph"
    mk evBus
    loop evBus


-- | make Hieroglyph render on the main window exposure
renderOnExpose :: VisualEventData a => Buster.Widget a
renderOnExpose busV = do
    bus <- takeMVar busV
    putMVar busV bus
    let runtimeE = fromJust $ Buster.eventByQName "Hieroglyph" "Hieroglyph" "RenderData" bus
        runtime = getHieroglyphData . Buster.eventdata $ runtimeE
        drawing = primitives . map (getGeo . getHieroglyphData . Buster.eventdata) $ drawingEs
        drawingEs = Set.toList $ Buster.eventsByGroup "Visible" bus

    runtime' <- render runtime drawing
    Buster.Insertion revent' <- Buster.produce "Hieroglyph" "Hieroglyph" "RenderData" Buster.Persistent (newHieroglyphData runtime')
    takeMVar busV
    let bus' = Buster.addEvent revent' bus
    putMVar busV bus'

-- | Make Hieroglyph send out expose events when it sees a (Hieroglyph,Hieroglyph,Rerender) event.
renderBehaviour :: VisualEventData a => Buster.Behaviour a
renderBehaviour bus = Buster.consumeFullyQualifiedEventWith bus "Hieroglyph" "Hieroglyph"  "Rerender" $ \event -> do
    GLUT.postRedisplay Nothing
    return $ []


-- | a behaviour to render hieroglyph data to the selection buffer when it sees a (Hieroglyph,Hieroglyph,PleaseSelect) event.
--   Produces (Selection,Hieroglyph,@objectname@) events.
selectionBehaviour :: VisualEventData a => Buster.Behaviour a
selectionBehaviour bus =
    case selectionRequested of
        Just sreq -> do -- print "Selection requested"
                        let (AttributedCoords selx sely _) = getHieroglyphData $ Buster.eventdata sreq
                        (p, GL.Size sx sy ) <- GL.get GL.viewport
                        GL.depthFunc $= Just GL.Less
                        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                        GL.matrixMode $= GL.Projection
                        GL.loadIdentity
                        GL.pickMatrix (selx-2, (fromIntegral sy)-sely+2) (6,6) (p, GL.Size sx sy)
                        maybe (GL.ortho 0 (fromIntegral sx) 0 (fromIntegral sy) 1 2)
                              (\(a,b,c,d) -> GL.ortho a b c d 1 2)
                              (ortho runtime)
                        (runtime', recs) <- GL.getHitRecords 16 $ renderObjects (Just (selx,sely)) [1::Double,2..] (sort drawing) runtime
                        selectionEvents <- forM (fromMaybe [] recs) $ \(GL.HitRecord x y names) ->
                            let names' = (fromMaybe "" . ((flip Map.lookup) (namemap runtime')) . (\(GL.Name x) -> x)) <$> names in do
                               Buster.produce "Selection" "Hieroglyph" (unlines names') Buster.once
                                (newHieroglyphData $ AttributedCoords (realToFrac x) (realToFrac y) names')

                        runtimeE' <- Buster.produce "Hieroglyph" "Hieroglyph" "RenderData" Buster.Persistent (setHieroglyphData runtime' runtimeE)
                        Buster.future bus . return $ [Buster.Deletion sreq , runtimeE'] ++ selectionEvents

        Nothing -> Buster.future bus . return $ []
    where runtimeE = Buster.eventdata  . fromJust $ Buster.eventByQName "Hieroglyph" "Hieroglyph" "RenderData" bus
          runtime = getHieroglyphData runtimeE
          drawing = primitives . map (getGeo . getHieroglyphData . Buster.eventdata) $ drawingEs
          drawingEs = Set.toList $ Buster.eventsByGroup "Visible" bus
          selectionRequested = Buster.eventByQName "Hieroglyph" "Hieroglyph" "PleaseSelect" bus

initializeBus :: VisualEventData a => String -> Int -> Int -> Buster.Widget a
initializeBus name w h bus = do
    let numTextures = 512
        numBufferObjects = 256

    GLUT.initialWindowSize $= GL.Size (fromIntegral w) (fromIntegral h)
    GLUT.getArgsAndInitialize 
    GLUT.initialDisplayMode $= [GLUT.RGBAMode, GLUT.DoubleBuffered, GLUT.Multisampling, GLUT.WithSamplesPerPixel 4, GLUT.WithDepthBuffer]
    GLUT.createWindow name
    GLUT.closeCallback $= Just (GLUT.leaveMainLoop)
    
    GLUT.displayCallback $= (GL.drawBuffer $= GL.BackBuffers >> renderOnExpose bus >> GLUT.swapBuffers) 

    GLUT.reshapeCallback $= Just (\(GL.Size w h) -> 
        GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral $ w) (fromIntegral $ h)))
        
    
    textures <- (GL.genObjectNames numTextures) :: IO [GL.TextureObject]
    buffers <- (GL.genObjectNames numBufferObjects) :: IO [GL.BufferObject]
    
    context <- Gtk.cairoCreateContext Nothing
    let edata = HgGLUT textures (Cache.empty (1024768*64) 0) [] buffers (Cache.empty (10247680*64) 0) [] Map.empty context Map.empty Nothing
    Buster.produce' "Hieroglyph" "Hieroglyph" "RenderData" Buster.Persistent (newHieroglyphData edata) bus

    return ()

