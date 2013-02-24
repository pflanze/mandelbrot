{-# LANGUAGE FlexibleInstances, BangPatterns, ScopedTypeVariables #-}

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Pixbuf
import Data.Word (Word8)
--import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Array.MArray --writeArray
import Data.Vector (generateM, forM_) -- .Unboxed

import Control.Monad
--import Control.Concurrent
import Control.Concurrent.Async
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


-- monadic for-each for a range

forM_0To :: Int -> (Int -> IO b) -> IO ()
forM_0To !end !m = for 0
  where for i = 
          if i < end then
            do m i
               for (i+1)
          else
            return ()

-- 'totally unsafe' parallel undeterministic execution of m in
-- parallel. Good or horrible idea?
parallelForM_0To :: Int -> (Int -> IO b) -> IO ()
parallelForM_0To !end !m = 
  do ids <- generateM end (\i -> async $ m i)
     Data.Vector.forM_ ids wait

-- missing function combinator

both f g a = f a && g a


-- Complex numbers

magnitudesquare !(r :+ i) = r*r + i*i

-- my own

myIterateUntil :: (Show a) => (a -> Bool) -> Int -> (a -> a) -> a -> (Int, a)
myIterateUntil !pred !maxdepth !fn !start = 
  iter maxdepth start
  where iter !0 !z = (maxdepth, z)
        iter !d !z = if notrace ((show d) ++ " -- " ++ (show z)) pred z then
                     (maxdepth-d,z)
                   else
                     iter (d-1) (fn z)

-- Mandelbrot series

pIter !c !z = z^2 + c

-- and its presentation

isDiverged !x = (magnitudesquare x) > (1e10**2)

mandelbrotDepth :: Int -> Complex Double -> Int
mandelbrotDepth !maxdepth !p =
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


depth = 200

renderScene :: WidgetClass widget => widget -> t -> IO Bool
renderScene d ev = do
  dw    <- widgetGetDrawWindow d
  (w, h) <- widgetGetSize d
  gc     <- gcNew dw
  -- pixbuf
  pb <- pixbufNew ColorspaceRgb False 8 w h
  pixels <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
  rowstride <- pixbufGetRowstride pb
  nChannels <- pixbufGetNChannels pb
  let setPoint !x !y !r !g !b =
        do writeArray pixels p r
           writeArray pixels (p+1) g
           writeArray pixels (p+2) b
        where p = y * rowstride + x * nChannels
 
  parallelForM_0To w
          (\_x -> 
            forM_0To h
            (\_y -> 
               let x = inscreen 0 w _x (-2.0) 1.0
                   y = inscreen 0 h _y (-1.0) 1.0
                   d = mandelbrotDepth depth (x :+ y)
               in
                notrace ((show depth)++"--("++(show x)++";"++(show y)++") "++(show d)) 
                (let l :: Word8 = fromIntegral (d * 255 `div` depth) in
                  setPoint _x _y l l l )))
  
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
