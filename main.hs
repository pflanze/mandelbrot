module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies


strictMap :: (a -> b) -> [a] -> [b]
strictMap fn (v:vs) =
  seq vs' (seq v' (v':vs'))
  where v' = fn v
        vs' = strictMap fn vs
strictMap fn [] = []

parallelMap :: (a -> b) -> [a] -> [b]
parallelMap fn (v:vs) =
  seq vs' (par v' (v':vs'))
  where v' = fn v
        vs' = parallelMap fn vs
parallelMap fn [] = []


strictAppend [] l = l
strictAppend (v:vs) l =
  seq vs' (v : vs')
  where vs' = strictAppend vs l


strictFlatten (l:ls) =
  strictAppend l (strictFlatten ls)
strictFlatten [] = []


chunked chunksize lis =
  ch chunksize lis
  where
  ch 0 vs = []:(ch chunksize vs)
  ch s (v:vs) = (v:lfront) : ls
    where (lfront:ls) = ch (s-1) vs
  ch s [] = [[]]


parallel_forM (i:is) fn =
  fn_i `par` (fn_is `pseq` (fn_i >> fn_is))
  where fn_i = fn i
        fn_is = parallel_forM is fn

parallel_forM [] fn =
  return ()

chunkedParallelForM chunksize is fn =
  parallel_forM (chunked chunksize is) (\is -> forM_ is fn)


chunkedParallelMap chunksize fn vs =
  strictFlatten $ parallelMap (\chunk -> strictMap fn chunk) (chunked chunksize vs)

intersection f g a = f a && g a

distsquare (a,b) = 
  a*a + b*b

isDiverged x = (distsquare x) > (1e10**2)

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


depth = 90

renderScene d ev = do
  dw    <- widgetGetDrawWindow d
  (w, h) <- widgetGetSize d
  gc     <- gcNew dw
  let fg = Color (65535 * 0)
                 (65535 * 0)
                 (65535 * 205)
  gcSetValues gc $ newGCValues { foreground = fg }
 
  let ll= parallelMap (\col -> strictMap (\row -> 
               let x = inscreen 0 w col (-2.0) 1.0
                   y = inscreen 0 h row (-1.0) 1.0
                   d = divergeDepth depth (mandelseries (x,y))
               in
                if d == depth then
                  drawPoint dw gc (col, row)
                else
                  return ()) [0..(h-1)]) [0..(w-1)]
   
  forM_ ll (\col -> forM_ col (\row -> row))

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
