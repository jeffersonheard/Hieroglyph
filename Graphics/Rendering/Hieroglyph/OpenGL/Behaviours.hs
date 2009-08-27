module Graphics.Rendering.Hieroglyph.OpenGL.Behaviours

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
import qualified App.Widgets.GtkMouseKeyboard as Buster
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import qualified Text.PrettyPrint as Pretty
import System.Mem.Weak

import Graphics.Rendering.Hieroglyph.OpenGL.Render

-- | Select based on mouse clicks
mouseSelectionBehaviour :: Buster.Behaviour [Buster.EData HieroglyphGLRuntime]
mouseSelectionBehaviour bus = Buster.pollFullyQualifiedEventWith bus "Mouse" "Hieroglyph.KeyboardMouseWidget" "SingleClick" $ \event -> do
    --print "MouseSelectionBehaviour"
    let (Buster.EAssocL alist) = head . Buster.eventdata $ event
        (Buster.EDoubleL (x:y:_)) = fromJust $ "coords" `lookup` alist
    --print "Leaving mouse selection behaviour"
    Buster.listM $ Buster.produce "Hieroglyph" "Hieroglyph" "PleaseSelect" Buster.once [Buster.EDouble x, Buster.EDouble y]



boilerplateOpenGLMain widgets behaviour = do
    evBus <- newMVar Buster.emptyBus
    forM_ widgets ($evBus)
    b <- takeMVar evBus
    putMVar evBus b

    let Just (Buster.EOther glarea) = fmap (head . Buster.eventdata) $ Buster.eventByQName "Hieroglyph" "Hieroglyph"  "RenderData" b
        loop mv = do
            Gtk.mainContextIteration Gtk.mainContextDefault True
            Buster.busIteration mv behaviour
            loop mv

    let mk = Buster.bindMouseKeyboardWidget (Gtk.castToWidget (window glarea))
    mk evBus
    loop evBus





-- | make Hieroglyph render on the main window exposure
renderOnExpose :: Buster.Widget [Buster.EData HieroglyphGLRuntime]
renderOnExpose busV = do
    bus <- takeMVar busV
    putMVar busV bus
    let runtimeE = fromJust $ Buster.eventByQName "Hieroglyph" "Hieroglyph" "RenderData" bus
        Buster.EOther runtime = head . Buster.eventdata $ runtimeE
        drawing = primitives . catMaybes . map getGeo . concat . map Buster.eventdata $ drawingEs
        drawingEs = Set.toList $ Buster.eventsByGroup "Visible" bus

    runtime' <- render  runtime drawing
    Buster.Insertion revent' <- Buster.produce "Hieroglyph" "Hieroglyph" "RenderData" Buster.Persistent [Buster.EOther runtime']
    takeMVar busV
    let bus' = Buster.addEvent revent' bus
    putMVar busV bus'


-- | Make Hieroglyph send out expose events when it sees a (Hieroglyph,Hieroglyph,Rerender) event.
renderBehaviour bus = Buster.consumeFullyQualifiedEventWith bus "Hieroglyph" "Hieroglyph"  "Rerender" $ \event -> do

    let Buster.EOther renderdata = head . Buster.eventdata . fromJust $ Buster.eventByQName "Hieroglyph" "Hieroglyph" "RenderData" bus
    (w,h) <- Gtk.widgetGetSize (window renderdata)
    Gtk.widgetQueueDrawArea (window renderdata) 0 0 w h

    return []


-- | a behaviour to render hieroglyph data to the selection buffer when it sees a (Hieroglyph,Hieroglyph,PleaseSelect) event.
--   Produces (Selection,Hieroglyph,@objectname@) events.
selectionBehaviour :: Buster.Behaviour [Buster.EData HieroglyphGLRuntime]
selectionBehaviour bus =
    case selectionRequested of
        Just sreq -> do -- print "Selection requested"
                        let [Buster.EDouble selx, Buster.EDouble sely] = Buster.eventdata sreq
                        (p, GL.Size sx sy ) <- GL.get GL.viewport
                        GL.matrixMode $= GL.Projection
                        GL.loadIdentity
                        GL.pickMatrix (selx-2, (fromIntegral sy)-sely+2) (6,6) (p, GL.Size sx sy)
                        GL.ortho2D 0 (fromIntegral sx) 0 (fromIntegral sy)
                        (runtime', recs) <- GL.getHitRecords 5 $ renderObjects [1::Double,2..] (sort drawing) runtime
                        selectionEvents <- forM (fromMaybe [] recs) $ \(GL.HitRecord x y names) ->
                            let names' = (fromMaybe "" . ((flip Map.lookup) (namemap runtime')) . (\(GL.Name x) -> x)) <$> names in
                            --print names
                            --print names'
                            Buster.produce "Selection" "Hieroglyph" (concat names') Buster.once
                                [Buster.EDouble . realToFrac $ x
                                , Buster.EDouble . realToFrac $ y
                                , Buster.EStringL $ names']

                        runtimeE' <- Buster.produce "Hieroglyph" "Hieroglyph" "RenderData" Buster.Persistent [Buster.EOther runtime']
                        Buster.future bus . return $ [Buster.Deletion sreq , runtimeE'] ++ selectionEvents

        Nothing -> Buster.future bus . return $ []
    where runtimeE = fromJust $ Buster.eventByQName "Hieroglyph" "Hieroglyph" "RenderData" bus
          Buster.EOther runtime = head . Buster.eventdata $ runtimeE
          drawing = primitives . catMaybes . map getGeo . concat . map Buster.eventdata $ drawingEs
          drawingEs = Set.toList $ Buster.eventsByGroup "Visible" bus
          selectionRequested = Buster.eventByQName "Hieroglyph" "Hieroglyph" "PleaseSelect" bus



-- | Widget for initializing the bus
initializeBus :: String -> Int -> Int -> Buster.Widget [Buster.EData HieroglyphGLRuntime]
initializeBus name w h bus = do
    let numTextures = 512
        numBufferObjects = 256

    print "Using latest version of Hieroglyph 1159"
    Gtk.unsafeInitGUIForThreadedRTS
    win <- Gtk.windowNew
    Gtk.windowSetTitle win name
    Gtk.widgetSetName win "Hieroglyph"
    Gtk.onDestroy win (exitWith ExitSuccess)
    Gtk.initGL
    config <- Gtk.glConfigNew [Gtk.GLModeRGBA, Gtk.GLModeMultiSample, Gtk.GLModeDouble, Gtk.GLModeDepth, Gtk.GLModeAlpha]
    area <- Gtk.glDrawingAreaNew config

    Gtk.onRealize area $ do
        GL.drawBuffer $= GL.BackBuffers

    Gtk.windowSetDefaultSize  win w h
    Gtk.containerResizeChildren win
    Gtk.containerAdd win area
    Gtk.widgetShowAll win
    (textures, buffers) <- Gtk.withGLDrawingArea area $ \_ -> do
        ts <- (GL.genObjectNames numTextures) :: IO [GL.TextureObject]
        bs <- (GL.genObjectNames numBufferObjects) :: IO [GL.BufferObject]
        return (ts,bs)

    context <- Gtk.cairoCreateContext Nothing

    let edata = HgGL textures (Cache.empty 1024768000 0) [] buffers (Cache.empty 1024768000 0) [] Map.empty area win context Map.empty
    Buster.produce' "Hieroglyph" "Hieroglyph" "RenderData" Buster.Persistent [Buster.EOther edata] bus

    Gtk.onExpose area (\_ -> renderOnExpose bus >> return True)
    return ()
