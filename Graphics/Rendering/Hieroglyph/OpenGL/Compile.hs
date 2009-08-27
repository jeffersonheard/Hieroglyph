{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
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

module Graphics.Rendering.Hieroglyph.OpenGL.Compile where

import Graphics.Rendering.Hieroglyph.OpenGL.Data
import qualified Graphics.Rendering.Hieroglyph.Cache as Cache
import System.Exit
import GHC.Float
import Data.List
import Control.Concurrent
import Control.Applicative
import Control.Exception
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

arcFn _ _ _ [] = []
arcFn x y r (t:ts) = (x + r * cos t) : (y + r * sin t) : arcFn x y r ts
arcVertices nvertices (Point cx cy) r t1 t2 = arcFn cx cy r $ [t1+t | t <- [0,(t2-t1)/nvertices..t2-t1]]

interleave (x:xs) (y:ys) = x:y:interleave xs ys
interleave [] _ = []
interleave _ [] = []

cubic a c0 c1 b ts = fmap interpolateCubic ts
   where interpolateCubic t = interpolate p20 p21 t
            where p20 = interpolate p10 p11 t
                  p21 = interpolate p11 p12 t
                  p10 = interpolate a c0 t
                  p11 = interpolate c0 c1 t
                  p12 = interpolate c1 b t
         interpolate x0 x1 t = x0 + ((x1 - x0)*t)

splineVertices nvertices (Point ax ay) (Point c0x c0y) (Point c1x c1y) (Point bx by) = interleave xs ys
    where xs = cubic ax c0x c1x bx [m / nvertices' | m <- [0 .. nvertices']]
          ys = cubic ay c0y c1y by [m / nvertices' | m <- [0 .. nvertices']]
          nvertices' = (fromIntegral nvertices) :: Double

compile e a@(Dots{}) = return $ compileDots e a
compile e a@(Arc{}) = return $ compileArc e a
compile e a@(Path{}) = return $ compilePath e a
compile e a@(Rectangle{}) = return $ compileRectangle e a
compile e a@(Text{}) = compileText e a
compile e a@(Image{}) = compileImage e a

compileDots e p@(Dots ds attrs s) = (maybe e (\n -> e{ namemap=Map.insert (fromIntegral s) n (namemap e)}) (aname attrs), CompiledDots p{attribs=attrs'} (vdata ds) (fromIntegral s))
    where vdata ((Point x y):vs) = x-tx:y-ty:vdata vs
          vdata [] = []
          attrs' = attrs{ atranslatex = atranslatex attrs + tx, atranslatey= atranslatey attrs + ty }
          Point tx ty = if (atranslatex attrs > 0 || atranslatey attrs > 0) && (not . null $ ds) then head ds else origin

compileArc e p@(Arc (Point cx cy) r t1 t2 reverse attrs sg) =  (maybe e (\n -> e{ namemap=Map.insert (fromIntegral sg) n (namemap e)}) (aname attrs),a0)
    where a0 = CompiledArc
                    p{attribs=attrs'}
                    (cx' : cy' : arcVertices 180
                                 p'
                                 r
                                 (if reverse then t2 else t1)
                                 (if reverse then t1 else t2))
                    (fromIntegral sg)
          attrs' = attrs{ atranslatex = atranslatex attrs + tx, atranslatey= atranslatey attrs + ty }
          Point tx ty = if (atranslatex attrs > 0 || atranslatey attrs > 0) then Point cx cy else origin
          p'@(Point cx' cy') =  if (atranslatex attrs > 0 || atranslatey attrs > 0) then origin else Point cx cy

compilePath e p =
    (maybe e
           (\n -> e{ namemap=Map.insert (fromIntegral (sig p)) n (namemap e) })
           (aname . attribs $ p)
    , CompiledPath p{attribs=attrs'} (fillablePath p) (fromIntegral (sig p)))

    where fillablePath p = pathOutline' (centroid (begin p:(ls2pt <$> segments p))) (Line (begin p): segments p)
          pathOutline p = if closed p then pathOutline' (begin p) (segments p ++ [Line $ begin p]) else pathOutline' (begin p) (segments p)
          pathOutline' (Point x0 y0) (Line (Point x1 y1) : ps) = [x0-tx,y0-ty,x1-tx,y1-ty] ++ pathOutline' (Point x1 y1) ps
          pathOutline' (Point x0 y0) (EndPoint (Point x1 y1) : ps) = pathOutline' (Point x1 y1) ps
          pathOutline' a (Spline c0 c1 b:ps) = splineVertices 64 (a-tp) (c0-tp) (c1-tp) (b-tp) ++ pathOutline' b ps
          pathOutline' _ [] = []
          attrs' = attrs{ atranslatex = atranslatex attrs + tx, atranslatey= atranslatey attrs + ty }
          tp@(Point tx ty) = if (atranslatex attrs > 0 || atranslatey attrs > 0) then begin p else origin
          attrs = attribs p

compileRectangle e p@(Rectangle (Point x y) w h attrs sg)  =
    (maybe e
           (\n -> e{ namemap=Map.insert (fromIntegral sg) n (namemap e) })
           (aname attrs)
    , CompiledRectangle p{ attribs=attrs'} x' y' w h (fromIntegral sg))
    where (x',y') = (x-tx,y-ty)
          Point tx ty = if (atranslatex attrs > 0 || atranslatey attrs > 0) then Point x y else origin
          attrs' = attrs{ atranslatex = atranslatex attrs + tx, atranslatey= atranslatey attrs + ty }

dataFrom (SB.PS d _ _) = d

nearestPowerOfTwo w h = (log2 $ wf, log2 $ hf)
    where log2 x = logDouble x / logDouble 2
          wf =  w
          hf =  h

getFreeTexture e = if texture_whitelist e /= []
    then (e{ texture_whitelist = tail $ texture_whitelist e }, head (texture_whitelist e))
    else (e{ texture_greylist = c' }, t)
    where ((_,t),c') = Cache.free . texture_greylist $ e



compileText e txt
 | cachetxt txt `Cache.member` texture_greylist e = do
    (_,GL.Size rx ry) <- GL.get GL.viewport
    let Point x y = bottomleft txt
        (w',h') = case (ortho e,astatic (attribs txt)) of
                        (Just (west,east,south,north),False) -> (w*(east-west)/fromIntegral rx, h*(north-south)/fromIntegral ry)
                        _ -> (w,h)
        (c', Just tex) = Cache.get (cachetxt txt) (texture_greylist e)
        (w,h) =  texdims e Map.! tex
    return ( e{ texture_greylist = c' }, CompiledImage txt{bottomleft = p', attribs=attrs'} cx' cy' w' h' tex (fromIntegral (sig txt)))

 | otherwise = do
    let (e', tex) = getFreeTexture e

    layout <- layoutEmpty (context e')
    layoutSetMarkup layout . Pretty.render . str $ txt
    layoutSetAlignment layout . align $ txt
    layoutSetJustify layout . justify $ txt
    layoutSetWidth layout . wrapwidth $ txt
    layoutSetWrap layout . wrapmode $ txt
    layoutSetIndent layout . indent $ txt
    (PangoRectangle _ _ _ _, PangoRectangle ex ey ew eh) <- layoutGetExtents layout
    let (po2w,po2h) = nearestPowerOfTwo ew eh
        potw = 2 ^ (max 0 $ ceiling po2w)
        poth = 2 ^ (max 0 $ ceiling po2h)
        w = fromIntegral potw
        h = fromIntegral poth
        Point x y = bottomleft txt

    textSurface <- Cairo.withImageSurface Cairo.FormatARGB32 potw poth $ \surf -> do
        Cairo.renderWith surf $ do
            Cairo.setOperator Cairo.OperatorSource
            maybe (Cairo.setSourceRGBA 1 1 1 0) (\colour -> let (a,b,c,d) = colourToTuple colour in Cairo.setSourceRGBA a b c d) (background txt)
            Cairo.rectangle 0 0 w h
            Cairo.fill
            Cairo.setOperator Cairo.OperatorOver
            Cairo.updateContext (context e')
            liftIO $ layoutContextChanged layout
            Cairo.save
            Cairo.translate (-ex) (-ey)
            let (fr,fg,fb,fa) = colourToTuple . afillRGBA $ state
                (sr,sg,sb,sa) = colourToTuple . astrokeRGBA $ state
                state = attribs txt
            when (afilled state) $ do
                Cairo.setSourceRGBA fr fg fb fa
                Cairo.showLayout layout
                Cairo.fill
            when (aoutlined state) $ do
                Cairo.setSourceRGBA sr sg sb sa
                Cairo.showLayout layout
                Cairo.stroke
            Cairo.restore
        Cairo.imageSurfaceGetData surf

    GL.textureBinding GL.Texture2D $= Just tex
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Clamp)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Clamp)
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    GL.textureFunction $= GL.Decal
    GL.texImage2D Nothing
        GL.NoProxy
        0
        GL.RGBA'
        (GL.TextureSize2D (fromIntegral potw) (fromIntegral poth))
        0
        (GL.PixelData GL.BGRA
        GL.UnsignedByte
        (unsafeForeignPtrToPtr (dataFrom textSurface)))

    (_,GL.Size rx ry) <- GL.get GL.viewport
    let (w',h') = case (ortho e',astatic (attribs txt)) of
                    (Just (west,east,south,north),False) -> (w*(east-west)/fromIntegral rx, h*(north-south)/fromIntegral ry)
                    _ -> (w,h)

    return ( e'{ texdims = Map.insert tex (w, h) (texdims e')
               , texture_greylist = Cache.put (cachetxt txt) tex (texture_greylist e')
               }
           , CompiledImage txt{bottomleft = p', attribs=attrs'} cx' cy' w' h' tex (fromIntegral (sig txt)))
 where attrs' = (attribs txt){ atranslatex = atranslatex attrs + tx, atranslatey= atranslatey attrs + ty, ascalex=1, ascaley=1 }
       Point tx ty = if (atranslatex attrs > 0 || atranslatey attrs > 0) then bottomleft txt else origin
       p'@(Point cx' cy') =  if (atranslatex attrs > 0 || atranslatey attrs > 0) then origin else bottomleft txt
       attrs = attribs txt

compileImage e img
 | cacheimg img `Cache.member` texture_greylist e = do
    (_,GL.Size rx ry) <- GL.get GL.viewport
    let (w',h') = case (ortho e,dimensions img,astatic (attribs img)) of
                        (Just (west,east,south,north),Left{},False) -> (w*(east-west)/fromIntegral rx, h*(north-south)/fromIntegral ry)
                        _ -> (w,h)
        (c',Just tex) =  Cache.get (cacheimg img) (texture_greylist e)
        (w,h) =  texdims e Map.! tex
    return ( e{texture_greylist = c'} , CompiledImage img x y w' h' tex (fromIntegral (sig img)))

 | otherwise = do
    (w,h,potw,poth,channels,buffer) <- case dimensions img of
        (Left (Point x y)) -> (Gtk.pixbufNewFromFile (filename img) >>= copydata) 
        (Right (Rect x y w h)) -> (Gtk.pixbufNewFromFile (filename img) >>= copydata) 
    if w > 0
     then do
        let (e', tex) = getFreeTexture e

        GL.textureBinding GL.Texture2D $= Just tex
        GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Clamp)
        GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Clamp)
        GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
        GL.textureFunction $= GL.Decal

        GL.texImage2D Nothing
            GL.NoProxy
            0
            (if channels == 4 then GL.RGBA' else GL.RGB')
            (GL.TextureSize2D (fromIntegral potw) (fromIntegral poth))
            0
            (GL.PixelData (if channels == 4 then GL.RGBA else GL.RGB)
            GL.UnsignedByte
            (unsafeForeignPtrToPtr (dataFrom buffer)))

        let (w',h') = case dimensions img of
                            Left _ -> (fromIntegral w, fromIntegral h)
                            Right (Rect _ _ w0 h0) -> (w0,h0)

        (_,GL.Size rx ry) <- GL.get GL.viewport
        let (w'',h'') = case (ortho e,dimensions img,astatic (attribs img)) of
                            (Just (west,east,south,north),Left{}, False) -> (w'*(east-west)/fromIntegral rx, h'*(north-south)/fromIntegral ry)
                            _ -> (w',h')

        return ( e'{ texdims = Map.insert tex (w',h') (texdims e')
                   , texture_greylist = Cache.put (cacheimg img) tex (texture_greylist e')
                   }
               , CompiledImage img x y w'' h'' tex (fromIntegral (sig img)))
     else return (e, CompiledRectangle rectangle 0 0 0 0 0)

 where (x, y) = case dimensions img of
                    Left (Point x0 y0) -> (x0, y0)
                    Right (Rect x0 y0 _ _) -> (x0,y0)


{-# INLINE copydata #-}
copydata pbuf0 = do
    w0 <- Gtk.pixbufGetWidth pbuf0
    h0 <- Gtk.pixbufGetHeight pbuf0
    let potw = log (fromIntegral w0) / log (fromIntegral 2)
        poth = log (fromIntegral h0) / log (fromIntegral 2)
        w = 2 ^ (max 0 $ ceiling potw)
        h = 2 ^ (max 0 $ ceiling poth)
    pbuf <- if w0 == w && h0 == h then return pbuf0 else Gtk.pixbufScaleSimple pbuf0 w h Gtk.InterpBilinear

    channels <- Gtk.pixbufGetNChannels pbuf
    bpc <- (`quot`8) <$> Gtk.pixbufGetBitsPerSample pbuf
    pixels <- Gtk.pixbufGetPixels pbuf :: IO (Gtk.PixbufData Int Word8)
    stride <- Gtk.pixbufGetRowstride pbuf
    buf <- SB.create (w*h*channels*bpc) $ \ptr ->
    --    forM_ [0::Int .. h - 1] $ \row -> let stsample = row*stride in
    --        forM_ [0::Int .. w*channels*bpc-1] $ \sample0 -> let sample = stsample + sample0 in
    --            A.readArray pixels sample >>= pokeByteOff ptr sample
        forM_ [0::Int .. w*h*channels*bpc - 1] $ \sample -> A.readArray pixels sample >>= pokeByteOff ptr sample
    return (w0,h0,w,h,channels,buf)

