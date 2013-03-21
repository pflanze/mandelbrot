{-# LANGUAGE FlexibleInstances, BangPatterns, ScopedTypeVariables, PackageImports #-}

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Pixbuf
import Data.Word (Word8)
--import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Array.MArray --writeArray
import "vector" Data.Vector (generateM, forM_) -- .Unboxed

import Data.Array.Repa                          as R

import Control.Monad
--import Control.Concurrent
import Control.Concurrent.Async
import Control.Parallel
import Control.Parallel.Strategies
import Data.Complex

-- Testing
import Test.QuickCheck
import System.Random
import Control.Applicative


-- monadic for-each for a range

forM_0To :: Int -> (Int -> IO b) -> IO ()
forM_0To !end !m = for 0
  where for !i = 
          if i < end then
            do m i
               for (i+1)
          else
            return ()

-- CAREFUL: undeterministic execution of m in parallel. 
parallelForM_0To :: Int -> (Int -> IO b) -> IO ()
parallelForM_0To !end !m = 
  do ids <- generateM end (\i -> async $ m i)
     Data.Vector.forM_ ids wait

-- Complex numbers

{-# INLINE magnitudesquare #-}
magnitudesquare !(r :+ i) = r*r + i*i

-- my own

{-# INLINE myIterateUntil #-}
myIterateUntil :: (Show a) => (a -> Bool) -> Int -> (a -> a) -> a -> (Int, a)
myIterateUntil !pred !maxdepth !fn !start = 
  iter maxdepth start
  where iter !0 !z = (maxdepth, z)
        iter !d !z = if pred z then
                     (maxdepth-d,z)
                   else
                     iter (d-1) (fn z)

-- Mandelbrot series

{-# INLINE pIter #-}
pIter !c !z = z^2 + c

-- and its presentation

{-# INLINE isDiverged #-}
isDiverged !x = (magnitudesquare x) > (1e10**2)

mandelbrotDepth :: Int -> Complex Double -> Int
mandelbrotDepth !maxdepth !p =
  fst $ myIterateUntil isDiverged maxdepth (pIter p) (0 :+ 0)


inscreen :: Int -> Int -> Int -> Double -> Double -> Double
inscreen from to i fromr tor =
  fromr + (tor - fromr) * (fromIntegral (i - from)) / (fromIntegral (to - from))


depth = 200
xfrom = (-2.0)
xto = 1.0
yfrom = (-1.0)
yto = 1.0


mandelbrotInscreen w h xfrom xto yfrom yto f (Z :. _x :. _y) = 
  let x = inscreen 0 w _x xfrom xto
      y = inscreen 0 h _y yfrom yto
      d = mandelbrotDepth depth (x :+ y)
  in
   let l :: Word8 = fromIntegral (d * 255 `div` depth) in
     l


calcMandelbrot w h =
  -- calculate
  let shape = Z :. w :. h -- :: (Shape Int Int)
      fakeinput = fromFunction shape (\_ -> 0 :: Word8)
      result = 
        --- XXX computeP $ 
        traverse fakeinput id (mandelbrotInscreen w h xfrom xto yfrom yto)
  in
   result


renderScene :: WidgetClass widget => widget -> t -> IO Bool
renderScene d ev = do
  dw    <- widgetGetDrawWindow d
  (w, h) <- widgetGetSize d
  gc     <- gcNew dw
  
  
  -- copy into bitmap
  -- pixbuf
  pb <- pixbufNew ColorspaceRgb False 8 w h
  pixels <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
  rowstride <- pixbufGetRowstride pb
  nChannels <- pixbufGetNChannels pb
  
  let result = computeP $ calcMandelbrot 10 20 
  
  let setPoint !x !y !r !g !b =
        do writeArray pixels p r
           writeArray pixels (p+1) g
           writeArray pixels (p+2) b
        where p = y * rowstride + x * nChannels
 
  -- parallelForM_0To w
  --         (\_x -> 
  --           forM_0To h
  --           (\_y -> 
  --               let l :: Word8 = fromIntegral (d * 255 `div` depth) in
  --                 setPoint _x _y l l l ))
  
  drawPixbuf dw gc pb 0 0 0 0 w h RgbDitherNone 0 0

  return True

main :: IO () 
main = do
  initGUI
  window  <- windowNew
  drawing <- drawingAreaNew
  windowSetTitle window "Mandelbrot"
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
