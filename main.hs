module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk hiding (Color, Point, Object)

import Control.Monad

renderScene d ev = do
 dw    <- widgetGetDrawWindow d
 (w, h) <- widgetGetSize d
 gc     <- gcNew dw
 let fg = Color (65535 * 0)
                (65535 * 0)
                (65535 * 205)
 gcSetValues gc $ newGCValues { foreground = fg }
 
 forM [10..140] (\x -> forM [40..200] (\y -> drawPoint dw gc (x, y)))
 return True

main :: IO () 
main = do
 initGUI
 window  <- windowNew
 drawing <- drawingAreaNew
 windowSetTitle window "Cells"
 containerAdd window drawing
 let bg = Color  (65535 * 0)
                 (65535 * 205)
                 (65535 * 255)
 widgetModifyBg drawing StateNormal bg
 onExpose drawing (renderScene drawing)
 
 onDestroy window mainQuit
 windowSetDefaultSize window 800 600
 windowSetPosition window WinPosCenter
 widgetShowAll window
 mainGUI
