{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6, 2011
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Convert image to sound file.
Spectrogram of result sound file resembles to input image.

-}
module Main where

import Data.Word (Word8)
import System.Environment (getArgs)

import Data.Array.Repa ((:.)(..), Array, DIM2, DIM3, Z(..), All(..))

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.IO.BMP as R
import qualified Data.Array.Repa.IO.Sndfile as R

main :: IO ()
main = do
  ipath:opath:_ <- getArgs
  img <- R.readImageFromBMP ipath
  case img of
    Left err   -> error $ show err
    Right img' -> R.writeSF opath R.wav16 (R.force $ go img')

go :: Array DIM3 Word8 -> Array DIM2 Double
go = squash . sumSins . toSins . to3D dt . dcbl . rgbfy
{-# INLINE go #-}

sr :: Int
sr = 48000
{-# INLINE sr #-}

dt :: Int
dt = 4800
{-# INLINE dt #-}

rgbfy :: Array DIM3 Word8 -> Array DIM2 (Word8, Word8, Word8)
rgbfy img = R.traverse img f g where
  f (Z:.i:.j:._) = Z:.i:.j
  g h ix = (h (ix:.0), h (ix:.1), h (ix:.2))
{-# INLINE rgbfy #-}

dcbl :: Array DIM2 (Word8, Word8, Word8) -> Array DIM2 Double
dcbl = R.map f where
  f (r,g,b) = (10 ** ((r'*0.21 + g'*0.71 + b'*0.07) / (255 * 3)) - 1) where
    (r',g',b') = (fromIntegral r, fromIntegral g, fromIntegral b)
{-# INLINE dcbl #-}

to3D :: Int -> Array DIM2 Double -> Array DIM3 Double
to3D n = R.extend (Z :. All :. All :. n)
{-# INLINE to3D #-}

toSins :: Array DIM3 Double -> Array DIM3 Double
toSins arr = R.traverse arr id f where
  _:.y:._:._ = R.extent arr
  f g ix@(_:.i:._:.k)
    | amp <= 0  = 0
    | otherwise = amp * sin (frq * k' * 2 * pi / sr') / y'
    where
      amp = g ix
      frq = (sr'/2) - (((y'-i'+1)/y') * (sr'/2))
      sr' = fromIntegral sr
      i' = fromIntegral i
      y' = fromIntegral y
      k' = fromIntegral k
{-# INLINE toSins #-}

sumSins :: Array DIM3 Double -> Array DIM2 Double
sumSins arr = R.sum (R.backpermute sh' f arr) where
  f (_:.i:.j:.k) = Z:.k:.j:.i
  sh' = Z:.z:.y:.x
  (_:.x:.y:.z) = R.extent arr
{-# INLINE sumSins #-}

squash :: Array DIM2 Double -> Array DIM2 Double
squash arr = R.backpermute sh' f arr where
  f (_:._:.j) = Z:.j `mod` x:.j `div` x
  (_:.x:.y) = R.extent arr
  sh' = Z:.1:.(x*y)
{-# INLINE squash #-}
