-- | 
-- 
-- Module      :  App.Widgets.MouseKeyboard
-- Copyright   :  (c) Renaissance Computing Institute 2009
-- License     :  BSD3
--
--   Gtk mouse keyboard widget.
--
--   For a mouse button press or release, add events named SingleClick or ClickRelease respectively to the bus.
--   For this widget, all events have source \"KeyboardMouseWidget\", and group \"Mouse\"
--   Additionally, the data attached to the event follows the form [EString SingleClick|ClickRelease, EDouble x, EDouble y, EStringL [Gtk modifier names]]
--
--   For a keyboard press or release, add events named KeyDown or KeyUp respectively to the bus.
--   All keyboard events have group ''Keyboard'' and source ''WidgetName.KeyboardMouseWidget''
--   Additionally, the data attached to a keyboard event follows the form [EString keyName | EChar keyChar, EStringL [Gtk modifier names]]
--
--   For a tablet proximity, add events named \"Proximity\" with source WidgetName.KeyboardMouseWidget, group \"Mouse\" and with attached data
--   [EBool True] for the tablet is in proximity and [EBool False] for the tablet is out of proximity.
--
--   For mouse motion, add events named \"Position\" with group \"Mouse\" and attached data [EDouble x, EDouble y, EStringL modifiers]
--
module App.Widgets.MouseKeyboardGLUT where

import Control.Applicative
import Control.Concurrent
import Data.Maybe
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import App.EventBus
import Graphics.Rendering.Hieroglyph.OpenGL.Data


keyboardMouseHandler wname b key keystate modifiers position = if isKey 
    then produce' "Mouse" (wname++".KeyboardMouseWidget") mousebuttonstate once (newHieroglyphData $ AttributedCoords (fromIntegral x) (fromIntegral y) (button : mods)) b
    else produce' "Keyboard" (wname++".KeyboardMouseWidget") keyboardkeystate once (newHieroglyphData $ AttributedCoords 0 0 (button : mods)) b
    where mousebuttonstate = if keystate == GLUT.Up then "ReleaseClick" else "SingleClick"
          keyboardkeystate = if keystate == GLUT.Up then "KeyUp" else "KeyDown" 
          button = case key of 
                    GLUT.MouseButton x -> show x
                    GLUT.Char c -> c:[]
                    GLUT.SpecialKey k -> show k
          isKey = case key of 
                    GLUT.MouseButton x -> False
                    GLUT.Char c -> True
                    GLUT.SpecialKey _ -> True
          mods = catMaybes [if GLUT.shift modifiers==GLUT.Down then Just "Shift" else Nothing
                           ,if GLUT.ctrl modifiers==GLUT.Down then Just "Control" else Nothing
                           ,if GLUT.alt modifiers==GLUT.Down then Just "Alt" else Nothing]
          (GL.Position x y) = position
          
motionHandler wname b (GL.Position x y) = do 
    produce' "Mouse" (wname ++ ".KeyboardMouseWidget") "Position" once (newHieroglyphData $ AttributedCoords (fromIntegral x) (fromIntegral y) []) b
          
scrollWheelHandler wname b evt = passthrough b



-- | Bind a keyboard mouse widget to the given Gtk widget. Se module documentation for description of events.
bindMouseKeyboardWidget :: VisualEventData a => String -> Widget a
bindMouseKeyboardWidget wname b = do
    GLUT.motionCallback $= Just (motionHandler wname b)
    GLUT.keyboardMouseCallback $= Just (keyboardMouseHandler wname b)
    return ()
