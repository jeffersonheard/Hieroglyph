{-# LANGUAGE BangPatterns, FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}
-- | 
 
-- Module      :  Graphics.Rendering.Hieroglyph.Primitives
-- Copyright   :  (c) Renaissance Computing Institute 2009
-- License     :  BSD3
--
--   This is Hieroglyph, a 2D scenegraph library similar in functionality to a barebones
--   stripped down version of Processing, but written in a purely functional manner.
--
--   See individual implementations (like the Graphics.Rendering.Hieroglyph.Cairo module)
--   for more information on how to use this library.
--
--      [@Author@] Jeff Heard
--
--      [@Copyright@] &copy; 2008 Renaissance Computing Institute
--
--      [@License@] A LICENSE file should be included as part of this distribution
--
--      [@Version@] 0.5
--
module Graphics.Rendering.Hieroglyph.Primitives where
import Graphics.UI.Gtk.Pango.Layout

import Control.Applicative ((<$>))

import System.IO.Unsafe
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.List
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB

import qualified Data.Map as Map
import qualified Data.IntMap as IM
import Text.PrettyPrint (Doc, (<>), (<+>), render, char, empty, text)
import qualified Text.PrettyPrint
import qualified Data.HashTable
import System.Random

deriving instance Eq LayoutWrapMode
deriving instance Show LayoutWrapMode
deriving instance Ord LayoutWrapMode
deriving instance Eq LayoutAlignment
deriving instance Show LayoutAlignment
deriving instance Ord LayoutAlignment
deriving instance Read LayoutWrapMode
deriving instance Read LayoutAlignment

instance Read Doc where
    readsPrec i = (\s -> [])

-- | A 2D point
data Point = Point Double Double deriving (Show, Read, Eq, Ord)

-- | Translate a point horizontally
xplus :: Double -> Point -> Point
xplus x (Point a b) = Point (x+a) b

-- | Translate a point vertically
yplus :: Double -> Point -> Point
yplus y (Point a b) = Point a (b+y)

pmap f (Point a b) = Point (f a) (f b)

comb f (Point x0 y0) (Point x1 y1) = Point (f x0 x1) (f y0 y1)

instance Num Point where
    (+) = comb (+)
    (*) = comb (*)
    (-) = comb (-)
    abs = pmap abs
    signum = pmap signum
    fromInteger x = let x' = fromInteger x in Point x' x'

instance Fractional Point where
    (/) = comb (/)
    fromRational a = Point (fromRational a) (fromRational a)

-- | Find the distance between two points
dist :: Point -> Point -> Double
dist (Point x0 y0) (Point x1 y1) = sqrt $ (x1-x0)**2 + (y1-y0)**2

-- | Find the average of a bunch of points
centroid :: [Point] -> Point
centroid ps = centroid' (Point 0 0) 0 ps
centroid' !s !n (p:ps) = centroid' (s+p) (n+1) ps
centroid' s n [] = s/n

-- | A rectangle for dimensions
data Rect = Plane | Singularity | Rect { x1 :: Double, y1 :: Double, x2 :: Double, y2 :: Double } deriving (Show, Read, Eq)

-- | A line segment
data LineSegment = Line Point | Spline Point Point Point | EndPoint Point deriving (Show,Read,Eq)

-- | A convenience function for getting points out of line segments in a path
ls2pt :: LineSegment -> Point
ls2pt (Line x) = x
ls2pt (Spline _ _ x) = x
ls2pt (EndPoint x) = x

instance Ord LineSegment where
    compare (Line p) (Line p') = compare p p'
    compare (EndPoint p) (EndPoint p') = compare p p'
    compare (Spline p q r) (Spline p' q' r') =
        fromMaybe EQ
        . find (/=EQ)
        . zipWith compare [p,q,r]
        $ [p',q',r']
    compare a b = (ordinal %=> compare) a b
        where ordinal (Line _) = 0
              ordinal (Spline _ _ _) = 1
              ordinal (EndPoint _) = 2

instance (Floating a, Ord a) => Ord (AlphaColour a) where
    compare a b = fromMaybe EQ . find (/=EQ) . zipWith compare [channelRed a', channelGreen a', channelBlue a'] $ [channelRed b', channelGreen b', channelBlue b']
        where a' = toSRGB $ if alphaChannel a == 0 then black else a `Data.Colour.over` black
              b' = toSRGB $ if alphaChannel b == 0 then black else b `Data.Colour.over` black

instance Ord Rect where
    compare Plane Plane = EQ
    compare Singularity Singularity = EQ
    compare Plane Singularity = GT
    compare Singularity Plane = LT
    compare (Rect _ _ _ _) Plane = GT
    compare (Rect _ _ _ _) Singularity = GT
    compare Plane (Rect _ _ _ _) = LT
    compare Singularity (Rect _ _ _ _) = LT
    compare (Rect xa1 ya1 xa2 ya2) (Rect xb1 xb2 yb1 yb2) =
        fromMaybe EQ
        . find (/=EQ)
        . zipWith compare [xa1,xa2,ya1,ya2]
        $ [xb1,xb2,yb1,yb2]

-- | Test to see if two rectangles overlap
overlaps :: Rect -> Rect -> Bool
overlaps _ Plane = True
overlaps Plane _ = True
overlaps _ Singularity = False
overlaps Singularity _ = False
overlaps (Rect lx1 ly1 lx2 ly2) (Rect rx1 ry1 rx2 ry2) = xoverlaps && yoverlaps
    where xoverlaps = (lx1' > rx1' && lx1' < rx2') || (lx2' > rx1' && lx2' < rx2')
          yoverlaps = (ly1' > ry1' && ly1' < ry2') || (ly2' > ry1' && ly2' < ry2')
          (lx1',lx2') = if lx1 < lx2 then (lx1,lx2) else (lx2,lx1)
          (ly1',ly2') = if ly1 < ly2 then (ly1,ly2) else (ly2,ly1)
          (rx1',rx2') = if rx1 < rx2 then (rx1,rx2) else (rx2,rx1)
          (ry1',ry2') = if ry1 < ry2 then (ry1,ry2) else (ry2,ry1)

-- | A 2D primitive in an arbitrary Cartesian 2d space
data Primitive =
      -- | A list of points that is renderable.
      Dots
        { at :: [Point]                                 -- The coordinates of the points in space
        , attribs :: Attributes                         -- The attributes of the points
        , sig :: Int
        }
      -- | An arc
    | Arc                                               -- A pie slice or arc
        { center   :: Point                             -- ^ center of the arc
        , radius   :: Double                            -- ^ radius of the arc
        , angle1   :: Double                            -- ^ begin angle
        , angle2   :: Double                            -- ^ end angle
        , negative :: Bool                              -- ^ whether or not to consider this a slice of or a slice out of the pie
        , attribs :: Attributes
        , sig :: Int
        }
    -- | A cubic spline
    | Path                                             -- An arbitrary line or cubic spline
        { begin    :: Point                             -- ^ starting point
        , segments :: [LineSegment]                     -- ^ A sequential list of curve segments.  Note that the first two points are control points.
        , closed   :: Bool                              -- ^ Whether or not to close this curve with a final line
        , attribs :: Attributes
        , sig :: Int
        }
    -- | A rectangle
    | Rectangle                                         -- An rectangle
        { topleft  :: Point                             -- ^ The top left point
        , width    :: Double                            -- ^ The width
        , height   :: Double                            -- ^ The height
        , attribs :: Attributes
        , sig :: Int
        }
    -- | A simple text object
    | Text                                              -- A simple text string
        { str        :: Doc                             -- ^ The string to print, in Pango markup format
        , bottomleft :: Point                           -- ^ The anchor point for the text.  Baseline, not bottom.
        , align :: LayoutAlignment
        , wrapwidth :: Maybe Double
        , wrapmode :: LayoutWrapMode
        , justify :: Bool
        , indent :: Double
        , attribs :: Attributes
        , spacing :: Double
        , background :: Maybe (AlphaColour Double)
        , sig :: Int
        }
   -- | Not a primitive shape, exactly, but the union of several primitives.  No order is implied in a union, merely that the areas that intersect are
    | Union
        { prims :: [Primitive]
        , attribs :: Attributes
        , sig :: Int
        }
    -- | A rectangular image
    | Image
        { filename :: String                -- ^ The filename of the image.  Should be something openable by Gdkpixbuf
        , dimensions :: Either Point Rect   -- ^ The dimensions of the image in current coordinates.  Either you use a point, and the image is full size, top left anchored to the point, or a rectangle
        , preserveaspect :: Bool            -- ^ Whether or not to scale preserving aspect ratio
        , attribs :: Attributes
        , sig :: Int
        }
    -- | A hidden item.  Used for state manipulation and to hide an object based on the current state
    | Hidden
        { attribs :: Attributes
        , sig :: Int
        }
    deriving (Show,Read,Eq)


instance Eq Doc where
    x == y = show x == show y

instance Ord Doc where
    compare = show %=> compare

data Attributes = Attributes
    { afillrule     :: FillRule                      -- ^ The pattern fill rule
    , afillRGBA     :: AlphaColour Double            -- ^ The components of the stroke color in the range [0..1]
    , adash         :: Maybe ([Double],Double)       -- ^ The shape of the line dashing, if any
    , astrokeRGBA   :: AlphaColour Double            -- ^ The components of the stroke color in the range [0..1]
    , aantialias    :: Antialias                     -- ^ The way things are antialiased
    , alinecap      :: LineCap                       -- ^ The way lines are capped
    , alinejoin     :: LineJoin                      -- ^ The way lines are joined
    , alinewidth    :: Double                        -- ^ The width of a line in points
    , amiterlimit   :: Double                        -- ^ The miter limit of lines.  See Cairo's documentation
    , atolerance    :: Double                        -- ^ The trapezoidal tolerance.  See Cairo's documentation
    , aoperator     :: Operator                      -- ^ The transfer operator.  See Cairo's documentation for more <http://cairographics.org>
    , atranslatex   :: Double                        -- ^ The current translation x component
    , atranslatey   :: Double                        -- ^ The current translation y component
    , ascalex       :: Double                        -- ^ The current scale x component
    , ascaley       :: Double                        -- ^ The current scale y component
    , arotation     :: Double                        -- ^ The rotation in degrees that this primitive is seen in
    , afilled       :: Bool                          -- ^ Whether or not this primitive is filled in
    , aoutlined     :: Bool                          -- ^ Whether or not this primitive is outlined
    , aclipped      :: Bool                          -- ^ Whether or not this primitive is part of the clipping plane
    , layer        :: Int                           -- ^ This sorts out which primitives are located on top of each other.  Do not set this yourself.  Use Graphics.Rendering.Hieroglyph.Visual.over
    , bbox         :: Rect                          -- ^ The clockwise rotation in radians.
    , aname        :: Maybe String                  -- ^ The name of the object
    , lod          :: Int                           -- ^ The level of detail that this primitive is at. Use Graphics.Rendering.Hieroglyph.Visual.moreSpecific
    , updated      :: Bool
    , styleselector :: Maybe String
    , astatic        :: Bool
    }
    deriving (Show,Read,Eq)

g %=> f = f `on` g

-- | define some instance of Ord over attributes that compares attribute sets
--   based on the occlusion layer and rendering cost of setting two primitives
--   next to one another.
instance Ord Attributes where
    compare a b = fromMaybe EQ $ find (/=EQ) . map ($(a,b)) . map uncurry $ [layer %=> compare, aname %=>compare]

-- | define a total ordering over the primitives based on layer and rendering cost
instance Ord Primitive where
    compare a b =
        case (cmpattrs, cmpsigs, cmpprims) of
            (EQ, EQ, x) -> x
            (EQ, x, _) -> x
            (x, _, _) -> x
        where cmpattrs = (attribs %=> compare) a b
              cmpsigs = (sig %=> compare) a b
              cmpprims = comparePrimitives a b

comparePrimitives (Dots ats0 _ _) (Dots ats1 _ _) = maybe EQ (uncurry compare) $ find (\(a,b) -> a /= b) (zip ats0 ats1)
comparePrimitives (Arc c r a1 a2 _ _ _) (Arc c' r' a1' a2' _ _ _) = fromMaybe EQ (find (/=EQ) [compare c c', compare r r', compare a1 a1', compare a2 a2'])
comparePrimitives (Path beg segs _ _ _) (Path beg' segs' _ _ _) = fromMaybe EQ (find (/=EQ) (compare beg beg' : compare (length segs) (length segs') : map (uncurry compareSegment) (zip segs segs')))
    where compareSegment (Line a) (Line b) = compare a b
          compareSegment (Spline a a1 a2) (Spline b b1 b2) = fromMaybe EQ (find (/=EQ) [compare a b, compare a1 b1, compare a2 b2])
          compareSegment (EndPoint a) (EndPoint b) = compare a b
          compareSegment a b = compare (lineordering a) (lineordering b)
          lineordering (Line _) = 0
          lineordering (Spline _ _ _) = 1
          lineordering (EndPoint _) = 2
comparePrimitives (Rectangle o w h _ _) (Rectangle o' w' h' _ _) = fromMaybe EQ (find (/=EQ) [compare o o', compare w w', compare h h'])
comparePrimitives (Text s b _ _ _ _ _ _ _ _ _) (Text s' b' _ _ _ _ _ _ _ _ _) = fromMaybe EQ (find (/=EQ) [compare s s', compare b b'])
comparePrimitives (Union p _ _) (Union p' _ _) = fromMaybe EQ . find (/=EQ) . map (uncurry compare) $ zip p p'
comparePrimitives (Image f d p _ _) (Image f' d' p' _ _) = fromMaybe EQ . find (/=EQ) $ [compare f f', compare d d', compare p p']
comparePrimitives a b = compare (primitiveOrdering a) (primitiveOrdering b)

-- comparePrimitives (Text _ _ _ _ _ _ _ _ _ _) (Text _ _ _ _ _ _ _ _ _ _) =

primitiveOrdering (Dots _ _ _) = 0
primitiveOrdering (Arc _ _ _ _ _ _ _) = 1
primitiveOrdering (Path _ _ _ _ _) = 2
primitiveOrdering (Rectangle _ _ _ _ _) = 3
primitiveOrdering (Text{}) = 4
primitiveOrdering (Union _ _ _) = 5
primitiveOrdering (Image _ _ _ _ _) = 6
primitiveOrdering (Hidden _ _) = 7

-- | See the Cairo meanings of these.  I plan to introduce OpenGL equivalents
data Antialias =
      AntialiasDefault
    | AntialiasNone
    | AntialiasGray
    | AntialiasSubpixel
    deriving (Show,Read,Ord,Eq)

-- | See the Cairo meanings of these.  I plan to introduce OpenGL equivalents
data FillRule =
      FillRuleWinding
    | FillRuleEvenOdd
    deriving (Show,Read,Ord,Eq)

-- | See the Cairo meanings of these.  I plan to introduce OpenGL equivalents
data LineCap =
      LineCapButt
    | LineCapRound
    | LineCapSquare
    deriving (Show,Read,Ord,Eq)

-- | See the Cairo meanings of these.  I plan to introduce OpenGL equivalents
data LineJoin =
      LineJoinMiter
    | LineJoinRound
    | LineJoinBevel
    deriving (Show,Read,Ord,Eq)

-- | See the Cairo meanings of these.  I plan to introduce OpenGL equivalents
data Operator =
      OperatorClear
    | OperatorSource
    | OperatorOver
    | OperatorIn
    | OperatorOut
    | OperatorAtop
    | OperatorDest
    | OperatorDestOver
    | OperatorDestIn
    | OperatorDestOut
    | OperatorDestAtop
    | OperatorXor
    | OperatorAdd
    | OperatorSaturate
    deriving (Show,Read,Ord,Eq)

-- | The default primitive attributes.  See source code for more details.
plain :: Attributes
plain =
    Attributes
        { afillrule = FillRuleWinding
        , afillRGBA = opaque white
        , adash = Nothing
        , astrokeRGBA = opaque white
        , aantialias = AntialiasDefault
        , alinecap = LineCapButt
        , alinejoin = LineJoinMiter
        , alinewidth = 1
        , amiterlimit = 0
        , atolerance = 0.1
        , aoperator = OperatorOver
        , atranslatex = 0
        , atranslatey = 0
        , ascalex = 1
        , ascaley = 1
        , arotation = 0
        , aoutlined = True
        , aclipped = False
        , afilled = False
        , bbox = Plane
        , layer = 0
        , lod = 0
		, aname = Nothing
		, updated = True
        , styleselector = Nothing
        , astatic = False }


-- | See pango span tag
tspan :: String -> Doc -> Doc
tspan a s = text "<span " <> text a <> char '>' <> s <> text "</span>"

mark m d = char '<' <> text m <> char '>' <> d <> text "</" <> text m <> char '>'

-- | See pango layout bold tag
bold :: Doc -> Doc
bold = mark "b"

-- | See pango layout bigger tag
bigger :: Doc -> Doc
bigger = mark "big"

-- | See pango layout italic tag
italic :: Doc -> Doc
italic = mark "i"

-- | See pango layout strikethrough tag
strikethrough :: Doc -> Doc
strikethrough = mark "s"

-- | See pango layout superscript tag
subscript :: Doc -> Doc
subscript = mark "sub"

-- | See pango layout superscript tag
superscript :: Doc -> Doc
superscript = mark "sup"

-- | See pango layout smaller tag
smaller :: Doc -> Doc
smaller = mark "small"

-- | See pango layout monospace tag
monospace :: Doc -> Doc
monospace = mark "tt"

-- | See pango layout underline tag
underline :: Doc -> Doc
underline = mark "u"

-- end Text.PrettyPrint combinators.

nguid :: () -> Int -- GAAAH!!! Just name your objects, folks, please.
nguid () = unsafePerformIO $ getStdRandom (randomR (0,2147483648))

sign :: Primitive -> Primitive
sign x = x{ sig = fromMaybe (nguid ()) (fromIntegral . Data.HashTable.hashString <$> (aname . attribs $ x)) }

-- | the origin point
origin :: Point
origin = Point 0 0

points :: Primitive
points = sign $ Dots [] plain 0

arc :: Primitive -- ^ A unit circle by default, modifiable with record syntax.
arc = sign $ Arc origin 1 0 (2*pi) False plain 0

path :: Primitive -- ^ A line starting at the origin.
path = sign $ Path origin [] False plain 0

polygon :: Primitive -- ^ An arbitrary filled polygon starting at the origin.
polygon = sign $ Path origin [] True plain{ afilled=True } 0

rectangle :: Primitive -- ^ an outlined rectangle
rectangle = sign $ Rectangle (Point 0 1) 1 1 plain 0

string :: Primitive -- ^ A rendered string starting at the origin.
string = sign $ Text empty origin AlignLeft Nothing WrapWholeWords False 0 plain{ afilled=True } 0 Nothing 0

compound :: Primitive -- ^ An outlined compound object
compound = sign $ Union [] plain 0

degrees :: Double -> Double -- ^ Convert degrees to radians
degrees x = x * 0.0174532925

hidden :: Primitive -- ^ A hidden object.
hidden = sign $ Hidden plain 0

image :: Primitive -- ^ An image.  These are efficiently cached where possible
image = sign $ Image "" (Left origin) False plain 0

textExtents context txt = unsafePerformIO $ do
    layout <- layoutEmpty context
    layoutSetMarkup layout . render . str $ txt
    layoutSetAlignment layout . align $ txt
    layoutSetJustify layout . justify $ txt
    layoutSetWidth layout . wrapwidth $ txt
    layoutSetWrap layout . wrapmode $ txt
    layoutSetIndent layout . indent $ txt
    (PangoRectangle _ _ _ _, PangoRectangle x y w h) <- layoutGetExtents layout
    return (x,y,w,h)
