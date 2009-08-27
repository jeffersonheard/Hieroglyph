-- |
 
-- Module      :  App.DebugEventBus
-- Copyright   :  (c) Renaissance Computing Institute 2009
-- License     :  BSD3
--
-- Declarative stylesheets for Hieroglyph
--
module Graphics.Rendering.Hieroglyph.Stylesheets 
    (Styling (StyleSelector, ArcStyle, DotsStyle, PathStyle, RectStyle, TextStyle)
    ,stylesheet
    ,styledPrims
    ,withStylesheet
    ,selectors
    ,arcstyle
    ,dotstyle
    ,pathstyle
    ,textstyle
    ,Stylesheet)
where

import Graphics.Rendering.Hieroglyph.Primitives
import Graphics.Rendering.Hieroglyph.Visual
import Data.Map (Map)
import qualified Data.Map as Map

-- | Basic type of a style combinator
type Style = BaseVisual -> BaseVisual
	
-- | Styling individual primitives	
data Styling =
      StyleSelector String Style
    | ArcStyle Style
    | DotsStyle Style
    | PathStyle Style
    | RectStyle Style
    | TextStyle Style

-- | A complete stylesheet
data Stylesheet = Stylesheet
    { selectors :: Map String Style -- ^ A list of style selectors that can be selected via the @style@ combinator in Graphics.Rendering.Hieroglyph.Visual
    , arcstyle :: Style -- ^ The style to apply to arc objects
    , dotstyle :: Style -- ^ The style to apply to dots primitives
    , pathstyle :: Style -- ^ The default style to apply to path primitives
    , rectanglestyle :: Style -- ^ The default style to apply to rectangle primitives
    , textstyle :: Style -- ^ The default style to apply to text primitives
    }

data BaseSel =
      IsArc (Maybe String) 
    | IsDots (Maybe String)
    | IsPath (Maybe String)
    | IsRect (Maybe String)
    | IsText (Maybe String)
    | IsRest (Maybe String)
    deriving (Ord,Eq)

-- | Compose stylesheet from Styling information
stylesheet :: [Styling] -> Stylesheet
stylesheet styles = foldr mkStylesheet' (Stylesheet Map.empty id id id id id) styles
    where mkStylesheet' (ArcStyle s)  ss       = ss{ arcstyle = s }
          mkStylesheet' (DotsStyle s) ss       = ss{ dotstyle = s }
          mkStylesheet' (PathStyle s) ss       = ss{ pathstyle = s }
          mkStylesheet' (RectStyle s) ss       = ss{ rectanglestyle = s }
          mkStylesheet' (TextStyle s) ss       = ss{ textstyle = s }
          mkStylesheet' (StyleSelector k s) ss = ss{ selectors = Map.insert k s . selectors $ ss }
binHelper p k = maybe
    (k Nothing,p)
    (\s -> (k (Just s),p))
    (styleselector . attribs $ p)

bin p@(Dots{}) = binHelper p IsDots
bin p@(Arc{}) = binHelper p IsArc
bin p@(Path{}) = binHelper p IsPath
bin p@(Rectangle{}) = binHelper p IsRect
bin p@(Text{}) = binHelper p IsText
bin p = binHelper p IsRest

styledPrims :: Visual a => String -> a -> BaseVisual
styledPrims ss = style ss . primitives

withStylesheet :: Stylesheet -> BaseVisual -> BaseVisual
withStylesheet ss v = concat $ Map.elems styledVisuals
    where binnedVisuals = foldr (\(b,p) -> Map.insertWith (++) b [p]) Map.empty . map bin $ v
          styledVisuals = Map.mapWithKey applyStyle binnedVisuals
          applyStyle (IsArc s) ps = applyStyle' s (arcstyle ss) ps
          applyStyle (IsDots s) ps = applyStyle' s (dotstyle ss) ps
          applyStyle (IsPath s) ps = applyStyle' s (pathstyle ss) ps
          applyStyle (IsRect s) ps = applyStyle' s (rectanglestyle ss) ps
          applyStyle (IsText s) ps = applyStyle' s (textstyle ss) ps
          applyStyle (IsRest s) ps = applyStyle' s id ps
          applyStyle' (Just sel) f ps = maybe (f ps) ($ps) (Map.lookup sel . selectors $ ss)

