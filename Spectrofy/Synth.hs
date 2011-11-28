{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6, 2011
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Synthesis methods

-}
module Spectrofy.Synth 
  ( sinsyn
  , fftsyn
  ) where 

import Data.Complex (Complex(..), realPart)
import Data.Word (Word8)
import Data.List (foldl1')

import Data.Array.Repa ((:.)(..), Array, DIM2, DIM3, Z(..), All(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.FFTW as R

-- --------------------------------------------------------------------------
--
-- Manual sinusoids
--

-- | Sums up sinusoids manually.
-- 
-- Frequency is taken from y axis, time from x axis.
-- Amplitude for each frequency is result of luminated value in each pixel.
--
sinsyn :: Int -> Int -> Array DIM3 Word8 -> Array DIM2 Double
sinsyn rate delta = squash . sumSins . toSins rate . to3D delta . rgbamp
{-# INLINE sinsyn #-}

rgbamp :: Array DIM3 Word8 -> Array DIM2 Double
rgbamp arr = R.traverse arr f g where
  f (Z:.i:.j:._) = Z:.i:.j
  g h ix = 10 ** ((red*0.21 + green*0.71 + blue*0.07) / (255*3)) - 1 where
    red = fromIntegral $ h (ix:.0)
    green = fromIntegral $ h (ix:.1)
    blue = fromIntegral $ h (ix:.2)
{-# INLINE rgbamp #-}

to3D :: Int -> Array DIM2 Double -> Array DIM3 Double
to3D n = R.extend (Z:.All:.All:.n)
{-# INLINE to3D #-}

toSins :: Int -> Array DIM3 Double -> Array DIM3 Double
toSins rate arr = R.traverse arr id f where
  _:.y:._:._ = R.extent arr
  f g ix@(_:.i:._:.k)
    | amp <= 0  = 0
    | otherwise = amp * sin (frq * k' * 2 * pi / rate') / y'
    where
      amp = g ix
      frq = (rate'/2) - (((y'-i'+1)/y') * (rate'/2))
      rate' = fromIntegral rate
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

-- --------------------------------------------------------------------------
--
-- Inversed FFT
--

-- | Performs 1-dimensional inverse fft on each column, then concatenate results.
--  
-- RGB values are used for initial phases and magnitude.
--   
fftsyn :: Int -> Array DIM3 Word8 -> Array DIM2 Double
fftsyn fsz img =
  {-# SCC "fftsyn" #-}
  foldl1' R.append $ map (fftslice fsz img') [0..n-1] where
    img' = rgbamp img
    _:._:.n:._ = R.extent img
{-# INLINE fftsyn #-}

fftslice :: Int -> Array DIM2 Double -> Int -> Array DIM2 Double
fftslice wsize arr n =
  {-# SCC "fs_reshape" #-}
  R.reshape (Z :. 1 :. wsize :: DIM2) $
  {-# SCC "fs_map" #-}
  R.map realPart $
  {-# SCC "fs_ifft" #-}
  R.ifft $
  {-# SCC "fs_slice" #-}
  R.slice (R.map (\x -> x:+x) $ grow2d wsize (zpad arr)) (Z:.All:.n)
{-# INLINE fftslice #-}

grow2d :: Int -> Array DIM2 Double -> Array DIM2 Double
grow2d n arr = R.traverse arr f g where
  f _ = Z :. n :. y
  _:.x:.y = R.extent arr
  g h (_:.i:.j)
    | i' >= 4   = k/4
    | otherwise = h (Z:.i':.j)
    where
      k  = h (Z:.i'-3:.j) + h (Z:.i'-2:.j) + h (Z:.i'-1:.j) + h (Z:.i':.j)
      i' = fst $ properFraction $ (x' * fromIntegral i / n')
      n' = fromIntegral n :: Double
      x' = fromIntegral x
{-# INLINE grow2d #-}

zpad :: Array DIM2 Double -> Array DIM2 Double
zpad arr = {-# SCC "zpad" #-} R.reshape sh' arr' where
  _:.x:.y = R.extent arr
  len = x * y
  zeros = R.fromList zsh (replicate len 0)
  zsh = Z :. len
  sh' = Z :. (2*x) :. y
  arr' = R.append (R.reshape zsh arr) zeros
{-# INLINE zpad #-}
