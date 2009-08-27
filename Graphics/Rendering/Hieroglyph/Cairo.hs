-- | 
-- Module      :  Graphics.Rendering.Hieroglyph.Cairo
-- Copyright   :  (c) Renaissance Computing Institute 2009
-- License     :  BSD3
--
--
--      [@Author@] Jeff Heard
--
--      [@Copyright@] &copy; 2008 Renaissance Computing Institute
--
--      [@License@] A LICENSE file should be included as part of this distribution
--
--      [@Version@] 0.5
--
module Graphics.Rendering.Hieroglyph.Cairo where

import qualified Graphics.Rendering.Hieroglyph.Cache as Cache
import qualified Data.Set as Set
import qualified Graphics.UI.Gtk.Cairo as Gtk
import Graphics.UI.Gtk.Pango.Context
import Graphics.UI.Gtk.Pango.Layout
import Data.Map (Map)
import qualified Data.Map as M
import System.Mem.Weak
import Control.Concurrent
import Control.Monad.Trans (liftIO)
import Graphics.Rendering.Hieroglyph.Primitives
import Graphics.Rendering.Hieroglyph.Visual
import Graphics.UI.Gtk.Gdk.Pixbuf
import qualified Graphics.UI.Gtk.Cairo as Cairo
import qualified Graphics.Rendering.Cairo as Cairo
import Control.Monad
import Control.Monad.IfElse
import Data.Foldable (foldlM)
import Data.List (sort)
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names (black)
import qualified Text.PrettyPrint as Pretty

type ImageCache = MVar (Cache.Cache Primitive Pixbuf)


toCairoAntialias AntialiasDefault = Cairo.AntialiasDefault
toCairoAntialias AntialiasNone = Cairo.AntialiasNone
toCairoAntialias AntialiasGray = Cairo.AntialiasGray
toCairoAntialias AntialiasSubpixel = Cairo.AntialiasSubpixel

toCairoFillRule FillRuleWinding = Cairo.FillRuleWinding
toCairoFillRule FillRuleEvenOdd = Cairo.FillRuleEvenOdd

toCairoLineCap LineCapButt = Cairo.LineCapButt
toCairoLineCap LineCapRound = Cairo.LineCapRound
toCairoLineCap LineCapSquare = Cairo.LineCapSquare

toCairoLineJoin LineJoinMiter = Cairo.LineJoinMiter
toCairoLineJoin LineJoinRound = Cairo.LineJoinRound
toCairoLineJoin LineJoinBevel = Cairo.LineJoinBevel

toCairoOperator OperatorClear = Cairo.OperatorClear
toCairoOperator OperatorSource = Cairo.OperatorSource
toCairoOperator OperatorOver = Cairo.OperatorOver
toCairoOperator OperatorIn = Cairo.OperatorIn
toCairoOperator OperatorOut = Cairo.OperatorOut
toCairoOperator OperatorAtop = Cairo.OperatorAtop
toCairoOperator OperatorDest = Cairo.OperatorDest
toCairoOperator OperatorXor = Cairo.OperatorXor
toCairoOperator OperatorAdd = Cairo.OperatorAdd
toCairoOperator OperatorSaturate = Cairo.OperatorSaturate

colourToTuple :: AlphaColour Double -> (Double,Double,Double,Double)
colourToTuple c = (r,g,b,alpha)
    where alpha = alphaChannel c
          c' = (1/alpha) `darken` (c `Data.Colour.over` black)
          RGB r g b = toSRGB c'


fillStrokeAndClip state action = do
    let (fr,fg,fb,fa) = colourToTuple . afillRGBA $ state
        (sr,sg,sb,sa) = colourToTuple . astrokeRGBA $ state
    when (afilled state) $ Cairo.setSourceRGBA fr fg fb fa >> action >> Cairo.fill
    when (aoutlined state) $ Cairo.setSourceRGBA sr sg sb sa >> action >> Cairo.stroke
    when (aclipped state) $ Cairo.clip

renderCurveSegs (Line (Point x0 y0)) = Cairo.lineTo x0 y0
renderCurveSegs (EndPoint (Point x0 y0)) = Cairo.moveTo x0 y0
renderCurveSegs (Spline (Point x0 y0) (Point x1 y1) (Point x2 y2)) = Cairo.curveTo x0 y0 x1 y1 x2 y2

-- | @renderPrimitive state prim@ draws a single primitive.
renderPrimitive :: PangoContext -> ImageCache -> Attributes -> Primitive -> Cairo.Render Attributes
renderPrimitive _ _ s0 (Arc (Point cx cy) radius angle0 angle1 isnegative state _) = do
    applyAttributeDelta s0 state
    fillStrokeAndClip state $
        if isnegative then Cairo.arcNegative cx cy radius angle0 angle1 else Cairo.arc cx cy radius angle0 angle1
    return state

renderPrimitive _ _ s0 (Dots ats attrs sig) = do
    applyAttributeDelta s0 attrs
    fillStrokeAndClip attrs $ do
        forM_ ats $ \(Point ox oy) -> do
            Cairo.moveTo ox oy
            Cairo.arc ox oy (alinewidth attrs) 0 (2*pi)
    return attrs

renderPrimitive _ _ s0 (Path (Point ox oy) segs isclosed state _) = do
    applyAttributeDelta s0 state
    fillStrokeAndClip state $ do
        Cairo.moveTo ox oy
        forM_ segs $ renderCurveSegs
        when isclosed (Cairo.lineTo ox oy)
    return state

renderPrimitive _ images s0 i@(Image filename (Left (Point ox oy)) _ state _) = do
    applyAttributeDelta s0 state
    pbuf <- loadImage images i
    w <- liftIO $ pixbufGetWidth pbuf
    h <- liftIO $ pixbufGetHeight pbuf
    Cairo.save
    Cairo.setSourcePixbuf pbuf ox oy
    Cairo.rectangle ox oy (fromIntegral w) (fromIntegral h)
    Cairo.fill
    Cairo.restore
    return state

renderPrimitive _ images s0 i@(Image filename (Right (Rect ox oy w h)) _ state _) = do
    applyAttributeDelta s0 state
    pbuf <- loadImage images i
    Cairo.save
    Cairo.setSourcePixbuf pbuf ox oy
    Cairo.rectangle ox oy w h
    Cairo.fill
    Cairo.restore
    return state

renderPrimitive _ _ s0 (Hidden _ _) = return s0
renderPrimitive _ _ s0 (Rectangle (Point ox oy) w h state _) = do
    applyAttributeDelta s0 state
    fillStrokeAndClip state $ Cairo.rectangle ox oy w h
    return state

renderPrimitive context _ s0 txt@(Text _ (Point ox oy) _  _ _ _  _ _ _  _ _) = do
    layout <- liftIO $ layoutEmpty context >>= \layout -> do
                         layoutSetMarkup layout . Pretty.render . str $ txt
                         layoutSetAlignment layout . align $ txt
                         layoutSetJustify layout . justify $ txt
                         layoutSetWidth layout . wrapwidth $ txt
                         layoutSetWrap layout . wrapmode $ txt
                         layoutSetIndent layout . indent $ txt
                         return layout
    applyAttributeDelta s0 (attribs txt)
    fillStrokeAndClip (attribs txt) $ do
        Cairo.moveTo ox oy
        Cairo.showLayout layout
    return (attribs txt)

renderPrimitive context images s0 (Union prims state _) = do
    let unfoc prim = prim{ attribs = (attribs prim){afilled=False, aoutlined=False, aclipped=False } }
    applyAttributeDelta s0 state
    fillStrokeAndClip state $ forM_ prims (renderPrimitive context images state . unfoc)
    return state

render context images d = loadStateIntoCairo attrs0
                    >> (foldlM (renderPrimitive context images) attrs0 . sort $ vis)
                    >> return ()
    where vis = sort $ primitives d
          attrs0 = attribs . head $ vis

applyAttributeDelta a b = do
    let different f = ((f %=> (/=)) a b)
        whendifferent f = when (different f)
    whendifferent afillrule . Cairo.setFillRule . toCairoFillRule . afillrule $ b
    whendifferent adash . maybe (return ()) (uncurry Cairo.setDash) . adash $ b
    whendifferent aantialias . Cairo.setAntialias . toCairoAntialias . aantialias $ b
    whendifferent alinewidth . Cairo.setLineWidth . alinewidth $ b
    whendifferent alinecap . Cairo.setLineCap . toCairoLineCap . alinecap $ b
    whendifferent alinejoin . Cairo.setLineJoin . toCairoLineJoin . alinejoin $ b
    whendifferent amiterlimit . Cairo.setMiterLimit .  amiterlimit $ b
    whendifferent atolerance . Cairo.setTolerance . atolerance $ b
    whendifferent aoperator . Cairo.setOperator . toCairoOperator . aoperator $ b
    when (different ascalex || different ascaley || different arotation || different atranslatex || different atranslatey) $ do
        Cairo.translate (atranslatex b) (atranslatey b)
        Cairo.scale (ascalex b) (ascaley b)
        Cairo.rotate (arotation b)
    return b

-- | Load the Cairo state  with a 'RenderState' Drawing.
loadStateIntoCairo :: Attributes -> Cairo.Render ()
loadStateIntoCairo s = do
    Cairo.setFillRule . toCairoFillRule . afillrule $ s
    awhen (adash s) $ \(a,b) -> Cairo.setDash a b
    Cairo.setAntialias . toCairoAntialias . aantialias $ s

    Cairo.setLineJoin . toCairoLineJoin . alinejoin $ s
    Cairo.setLineWidth . alinewidth $ s
    Cairo.setMiterLimit . amiterlimit $ s
    Cairo.setTolerance . atolerance $ s
    Cairo.setOperator . toCairoOperator . aoperator $ s

    Cairo.translate (atranslatex s) (atranslatey s)
    Cairo.scale (ascalex s) (ascaley s)
    Cairo.rotate (arotation s)

-- | @renderFrameToSurface surface frame@ renders a frame to a particular surface
renderToSurfaceWithImageCache :: Visual t => PangoContext -> ImageCache -> Cairo.Surface -> t -> IO ()
renderToSurfaceWithImageCache context images surf frame = Cairo.renderWith surf (render context images frame)

renderToSurface :: Visual t => PangoContext -> Cairo.Surface -> t -> IO ()
renderToSurface c s o = do { i <- newMVar (Cache.empty 1024 33) ;renderToSurfaceWithImageCache c i s o }

-- | @renderframeToPNGWithImageCache  filename xres yres frame@ renders a frame to an image file
renderToPNGWithImageCache  :: Visual t => PangoContext -> ImageCache -> FilePath -> Int -> Int -> t -> IO ()
renderToPNGWithImageCache c images filename xres yres frame = Cairo.withImageSurface Cairo.FormatARGB32 xres yres $ \s -> renderToSurfaceWithImageCache c images s frame >> Cairo.surfaceWriteToPNG s filename
renderToPNG f w h o = do { c <- Gtk.cairoCreateContext Nothing ;   i <- newMVar (Cache.empty 1024 33) ; renderToPNGWithImageCache c i f w h o }

-- | @renderToPDFWithImageCache  filename width height frame@ renders a frame to a PDF file.  width and height are in points.
renderToPDFWithImageCache  :: Visual t => PangoContext -> ImageCache -> FilePath -> Double -> Double -> t -> IO ()
renderToPDFWithImageCache c images filename width height frame = Cairo.withPDFSurface filename width height $ \s -> renderToSurfaceWithImageCache c images s frame
renderToPDF f w h o = do { c <- Gtk.cairoCreateContext Nothing ; i <- newMVar (Cache.empty 1024 33) ; renderToPDFWithImageCache c i f w h o }

-- | @renderToPostscriptWithImageCache  filename width height frame@ renders a frame to a Postscript file.  width and height are in points.
renderToPostscriptWithImageCache  :: Visual t => PangoContext -> ImageCache -> FilePath -> Double -> Double -> t -> IO ()
renderToPostscriptWithImageCache  c images filename width height frame = Cairo.withPSSurface filename width height $ \s -> renderToSurfaceWithImageCache c images s frame
renderToPostscript f w h o = do { c <- Gtk.cairoCreateContext Nothing ; i <- newMVar (Cache.empty 1024 33) ; renderToPostscriptWithImageCache c i f w h o }

-- | @renderToSVGWithImageCache  filename width height frame@ renders a frame to a SVG file.  width and height are in points.
renderToSVGWithImageCache  :: Visual t => PangoContext -> ImageCache -> FilePath -> Double -> Double -> t -> IO ()
renderToSVGWithImageCache c images filename width height frame = Cairo.withSVGSurface filename width height $ \s -> renderToSurfaceWithImageCache c images s frame
renderToSVG f w h o = do { c <- Gtk.cairoCreateContext Nothing ; i <- newMVar (Cache.empty 1024 33) ; renderToSVGWithImageCache c i f w h o }

-- | @loadImage dictRef image@ pulls an image out of the cache's hat.
loadImage :: ImageCache -> Primitive -> Cairo.Render (Pixbuf)
loadImage dictRef im@(Image filename (Right (Rect x y w h)) aspect _ _) = do
  liftIO $ modifyMVar dictRef $ \dict ->
    if im `Cache.member` dict
      then do let (cache',value) = Cache.get im dict
              pbuf <- case value of
                          Just pb -> return pb
                          Nothing -> pixbufNewFromFileAtScale filename (round w) (round h) aspect
              return (cache',pbuf)
      else do pbuf <- pixbufNewFromFileAtScale filename (round w) (round h) aspect
              return ((Cache.put im pbuf dict), pbuf)
loadImage dictRef im@(Image filename (Left (Point x y)) _ _ _) = do
  liftIO $ modifyMVar dictRef $ \dict ->
    if im `Cache.member` dict
      then do let (cache',value) = Cache.get im dict
              pbuf <- case value of
                          Just pb -> return pb
                          Nothing -> pixbufNewFromFile filename
              return (dict,pbuf)
      else do pbuf <- pixbufNewFromFile filename
              return  ((Cache.put im pbuf dict), pbuf)

