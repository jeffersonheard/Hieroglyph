{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Here I describe and define the class Visual and some example instances
--   for the basic data structures in the GHC standard library, including
--   Data.Map, Data.IntMap, Data.Set, and Lists.  This gives a rich library
--   of data structures that are Visual as a direct transformation from
--   forall a, Visual b : a -> Visual b
module Graphics.Rendering.Hieroglyph.Visual where

import Data.Colour
import Data.Monoid
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
-- import Data.Foldable
import Data.List
import Graphics.Rendering.Hieroglyph.Primitives

type BaseVisual = [Primitive]

-- | A Visual is an unstructured collection of primitives.  Conceptually, the
--   only requirement of a Visual is that it is Enumerable (or Foldable) in
--   terms of Primitives.  I initially wanted to implement this in terms of
--   the Foldable typeclass from Data.Foldable, but very few things provide
--   instances of Foldable that are conceptually so.  A list is Foldable,
--   and I have certain guarantees of efficiency by operating on lists, so
--   this is the instance of Foldable that I choose to have people implement.
class Visual t where
    primitives :: t -> BaseVisual

-- | A Primitive by itself is a Visual.  That is, a single piece of geometry
--   standing alone is in fact Visual.
instance Visual Primitive where
	primitives a = [a]
	
-- | A any list of Visuals can be flattenend into a list of Primitives by
--   a right fold, therefore a List of Visuals is a Visual
instance Visual a => Visual [a]	where
    primitives x = concat $ primitives `map` x

-- | A map to Visuals is Foldable over the elements in terms of Visuals, which
--   are in turn Foldable in terms of Primitives, therefore a map of
instance Visual b => Visual (M.Map a b) where
    primitives = primitives . M.elems

-- | An intmap is merely a map, so once again if the elements are Visuals, the
--   whole structure is a Visual.
instance Visual b => Visual (IM.IntMap b) where
	primitives = primitives . IM.elems

-- | A set of Visuals also comprises a Visual.
instance Visual t => Visual (S.Set t) where
	primitives = primitives . S.toList
	
-- | Declare that a Visual possibly occludes another Visual
occludes :: (Visual t, Visual u) => t -> u -> BaseVisual
occludes this that = ((\p -> p{ layer = maxlev+1 }) <%> primitives this) ++ (primitives that)
    where maxlev = maximum . map (layer . attribs) . primitives $ that

-- | Declare that a Visual doesn't occlude another Visual
beside :: (Visual t, Visual u) => t -> u -> BaseVisual
beside this that = ((\p -> p{ layer = maxlev }) <%> primitives this) ++ (primitives that)
    where maxlev = maximum . map (layer . attribs) . primitives $ that

pure x = [x]

f <%> x = map (\a -> a{ attribs = (f . attribs $ a)})  x

-- | The beside operator.  Same as @beside@
(#+#) :: BaseVisual -> BaseVisual -> BaseVisual
(#+#) = (++)

-- | The occludes operator.  The left occludes the right.
(#/#) :: BaseVisual -> BaseVisual -> BaseVisual
(#/#) = occludes

-- | The under operator.  The right occludes the left.
(#\#) :: BaseVisual -> BaseVisual -> BaseVisual
(#\#) = flip occludes

infixr 9 `occludes`
infixl 4 <%>
infixr 8 #+#
infixr 9 #/#
infixr 9 #\#

-- | Set the fill rule for this visual (Cairo)
fillrule :: Visual a =>  FillRule -> a -> BaseVisual
fillrule x os = (\o -> o{ afillrule = x }) <%> primitives os

-- | Set the fill colour
fillcolour :: Visual a =>  AlphaColour Double -> a -> BaseVisual
fillcolour x os = (\o -> o{ afillRGBA = x }) <%> primitives os

-- | Set the stipple pattern for lines (Cairo)
dash :: Visual a => Maybe ([Double],Double) -> a -> BaseVisual
dash x os = (\o -> o{adash = x}) <%> primitives os

-- | Set the stroke colour
strokecolour :: Visual a => AlphaColour Double -> a -> BaseVisual
strokecolour x os = (\o -> o{ astrokeRGBA = x }) <%> primitives os

-- | Set the line cap shape (Cairo)
linecap :: Visual a => LineCap -> a -> BaseVisual
linecap x os = (\o -> o{alinecap = x}) <%> primitives os

-- | Set the miter limit (Cairo)
miterlimit :: Visual a =>  Double -> a -> BaseVisual
miterlimit x os = (\o -> o{amiterlimit = x}) <%> primitives os

-- | Set the polygon tolerance (Cairo)
tolerance :: Visual a => Double -> a -> BaseVisual
tolerance x os = (\o -> o{atolerance = x}) <%> primitives os

-- | Set the scale of the Visual
scalex :: Visual a => Double -> a -> BaseVisual
scalex x os = (\o -> o{ascalex = x}) <%> primitives os

-- | Set the scale of the Visual
scaley :: Visual a => Double -> a -> BaseVisual
scaley x os = (\o -> o{ascaley = x}) <%> primitives os

-- | Adjust the scale of the visual
scale :: Visual a => Double -> Double -> a -> BaseVisual
scale x y os = (\o -> o{ascalex = x * ascalex o, ascaley = y * ascaley o})<%> primitives os

-- | Set the translation of the visual
settranslatex :: Visual a =>  Double -> a -> BaseVisual
settranslatex x os = (\o -> o{atranslatex = x })<%> primitives os

-- | Set the translation of the visual
settranslatey :: Visual a =>  Double -> a -> BaseVisual
settranslatey y os = (\o -> o{atranslatey = y })<%> primitives os

-- | Adjust the translation of the visual
translate ::  Visual a => Double -> Double -> a -> BaseVisual
translate x y os = (\o -> o{atranslatex = x + atranslatex o, atranslatey = y + atranslatey o})<%> primitives os

-- | Set the rotation of the visual
rotation ::  Visual a =>  Double -> a -> BaseVisual
rotation x os = (\o -> o{arotation = x}) <%> primitives os

-- | Set whether or not objects in the visual are filled
filled :: Visual a => Bool -> a -> BaseVisual
filled x os = (\o -> o{afilled = x}) <%> primitives os

-- | Set whether or not objects in the visual are outlined
outlined ::  Visual a => Bool -> a -> BaseVisual
outlined x os = (\o -> o{aoutlined = x}) <%> primitives os

-- | Set whether or not the objects in the visual are part of the clipping plane (Cairo)
clipped :: Visual a => Bool -> a -> BaseVisual
clipped x os = (\o -> o{aclipped = x}) <%> primitives os

-- | Set the name of the objects for selection purposes
name :: Visual a => String -> a -> BaseVisual
name x os = (\o -> o{aname = Just x}) <%> primitives os

-- | Set whether the objects have already been cached.
cached :: Visual a =>  a -> BaseVisual
cached os = (\o -> o{updated = False}) <%> primitives os

-- | Set whether the objects have been updated.
fresh :: Visual a => a -> BaseVisual
fresh os = (\o -> o{updated = True}) <%> primitives os

-- | Set the linewidth
linewidth :: Visual a => Double -> a -> BaseVisual
linewidth x os = (\o -> o{alinewidth = x}) <%> primitives os

-- | Set the stylesheet selector
style :: Visual a => String -> a -> BaseVisual
style s os = (\o -> o{ styleselector=Just s }) <%> primitives os

static :: Visual a =>  Bool -> a -> BaseVisual
static s os = (\o -> o{ astatic=s }) <%> primitives os
