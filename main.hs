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
 
 forM_ [10..140] (\i -> drawPoint dw gc (i, 120))
 drawPoint dw gc (22, 22)
 --drawRectangle dw gc True 20 20 20 20
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
