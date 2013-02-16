module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Control.Monad
import Control.Parallel
-- import Control.Concurrent.ParallelIO  http://hackage.haskell.org/package/parallel-io

parallel_forM (i:is) fn =
  fn_i `par` fn_is `par` pseq fn_i (fn_i >> fn_is)
  where fn_i = fn i
        fn_is = parallel_forM is fn

parallel_forM [] fn =
  return ()


intersection f g a = f a && g a

dist2 (a,b) = 
  a*a + b*b

isDiverged x = (dist2 x) > (1e10**2)

divergeDepth maxdepth seq =
  fst (head (dropWhile (intersection (not . isDiverged . snd)
                                     ((< maxdepth) . fst))
                       (zip [1..] seq)))

inscreen :: Int -> Int -> Int -> Double -> Double -> Double
inscreen from to i fromr tor =
  fromr + (tor - fromr) * (fromIntegral (i - from)) / (fromIntegral (to - from))

square :: (Double,Double) -> (Double,Double)
square (a,b) = 
  (a*a - b*b, 2*a*b)
  
plus :: (Double,Double) -> (Double,Double) -> (Double,Double)
plus (a1,a2) (b1,b2) = (a1+b1, a2+b2)

mandelseries :: (Double, Double) -> [(Double, Double)]

piter :: (Double,Double) -> (Double,Double) -> (Double,Double)
piter c z = plus (square z) c

mandel_ c z =  z : mandel_ c (piter c z)
mandelseries c = mandel_ c (0.0,0.0)


depth = 40

renderScene d ev = do
  dw    <- widgetGetDrawWindow d
  (w, h) <- widgetGetSize d
  gc     <- gcNew dw
  let fg = Color (65535 * 0)
                 (65535 * 0)
                 (65535 * 205)
  gcSetValues gc $ newGCValues { foreground = fg }
 
  parallel_forM [0..(w-1)] 
    (\col -> forM_ [0..(h-1)] 
             (\row -> 
               let x = inscreen 0 w col (-2.0) 1.0
                   y = inscreen 0 h row (-1.0) 1.0
                   d = divergeDepth depth (mandelseries (x,y))
               in
                if d == depth then
                  drawPoint dw gc (col, row)
                else
                  return ()))
   
  return True

main :: IO () 
main = do
  initGUI
  window  <- windowNew
  drawing <- drawingAreaNew
  windowSetTitle window "Cells"
  containerAdd window drawing
  let bg = Color (65535 * 0)
                 (65535 * 205)
                 (65535 * 255)
  widgetModifyBg drawing StateNormal bg
  onExpose drawing (renderScene drawing)
 
  onDestroy window mainQuit
  windowSetDefaultSize window 800 600
  windowSetPosition window WinPosCenter
  widgetShowAll window
  mainGUI
