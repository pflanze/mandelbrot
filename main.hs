{-# LANGUAGE FlexibleInstances #-}

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Pixbuf
import Data.Word (Word8)
--import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Array.MArray --writeArray


import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies
import Data.Complex
import Debug.Trace

-- Testing
import Test.QuickCheck
import System.Random
import Control.Applicative

--


notrace msg res = res


-- process control

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


-- monadic for-each for a range

forM_0To end m = for 0
  where for i = 
          if i < end then
            do m i
               for (i+1)
          else
            return ()
    

-- missing function combinator

both f g a = f a && g a


-- Complex numbers

magnitudesquare (r :+ i) = r*r + i*i

-- my own

myIterateUntil :: (Show a) => (a -> Bool) -> Int -> (a -> a) -> a -> (Int, a)
myIterateUntil pred maxdepth fn start = 
  iter maxdepth start
  where iter 0 z = (maxdepth, z)
        iter d z = if notrace ((show d) ++ " -- " ++ (show z)) pred z then
                     (maxdepth-d,z)
                   else
                     iter (d-1) (fn z)

-- Mandelbrot series

pIter c z = z^2 + c

-- and its presentation

isDiverged x = (magnitudesquare x) > (1e10**2)

mandelbrotDepth :: Int -> Complex Double -> Int
mandelbrotDepth maxdepth p =
  fst $ myIterateUntil isDiverged maxdepth (pIter p) (0 :+ 0)

-- OLD
mandelseries :: (Complex Double) -> [(Complex Double)]

piter :: (Complex Double) -> (Complex Double) -> (Complex Double)
piter c z = z^2 + c

mandel_ c z =  z' : mandel_ c z'
  where z'= piter c z
mandelseries c = mandel_ c (0.0 :+ 0.0)

divergeDepth maxdepth seq =
  fst (head (dropWhile (both (not . isDiverged . snd)
                             ((< maxdepth) . fst))
                       (zip [1..] seq)))

mandelbrotDepthOLD :: Int -> Complex Double -> Int
mandelbrotDepthOLD maxdepth p = 
  divergeDepth maxdepth (mandelseries p)

-- /OLD

inscreen :: Int -> Int -> Int -> Double -> Double -> Double
inscreen from to i fromr tor =
  fromr + (tor - fromr) * (fromIntegral (i - from)) / (fromIntegral (to - from))


depth = 90

renderScene d ev = do
  dw    <- widgetGetDrawWindow d
  (w, h) <- widgetGetSize d
  gc     <- gcNew dw
  -- pixbuf
  pb <- pixbufNew ColorspaceRgb False 8 w h
  pixels <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
  rowstride <- pixbufGetRowstride pb
  nChannels <- pixbufGetNChannels pb
  let setPoint x y r g b =
        do writeArray pixels p r
           writeArray pixels (p+1) g
           writeArray pixels (p+2) b
        where p = y * rowstride + x * nChannels
 
  let ll= chunkedParallelMap 20 
          (\_x -> 
            forM_0To h
            (\_y -> 
               let x = inscreen 0 w _x (-2.0) 1.0
                   y = inscreen 0 h _y (-1.0) 1.0
                   d = mandelbrotDepth depth (x :+ y)
               in
                notrace ((show depth)++"--("++(show x)++";"++(show y)++") "++(show d)) 
                      (if d == depth then
                         setPoint _x _y 100 100 100
                       else
                         return ())))
          [0..(w-1)]
   
  forM_ ll id
  
  drawPixbuf dw gc pb 0 0 0 0 w h RgbDitherNone 0 0

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
