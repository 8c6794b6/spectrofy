{-# LANGUAGE RecordWildCards #-}
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
import System.Console.GetOpt
import System.Environment (getArgs)

import Data.Array.Repa ((:.)(..), Array, DIM2, DIM3, Z(..), All(..))

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.IO.BMP as R
import qualified Data.Array.Repa.IO.Sndfile as R

main :: IO ()
main = spectrofy . parseOpts =<< getArgs

-----------------------------------------------------------------------------
-- Command line args

data Options = Options
  { showHelp :: Bool
  , samplingRate :: Int
  , numDelta :: Int
  } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
  { showHelp = False
  , samplingRate = 48000
  , numDelta = 4800 }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"]
    (NoArg (\flg -> flg {showHelp = True}))
    "Show this help"
  , Option ['r'] ["rate"]
    (ReqArg (\v flg -> flg {samplingRate = read v}) "INT")
    "Sampling rate (default 48000)"
  , Option ['d'] ["delta"]
    (ReqArg (\v flg -> flg {numDelta = read v}) "INT")
    "Number of delta samples (default 4800)"
  ]

usage :: String
usage = unlines [usageInfo msg options, example] where
  msg =
    "Usage: spectrofy [OPTIONS] [INFILE] [OUTFILE]\n\nOPTIONS:"
  example =
    "Example:\n\
    \  spectrofy -r 22050 -d 2200 in.bmp out.wav"

parseOpts :: [String] -> (Options, [String])
parseOpts args = case getOpt Permute options args of
  (fs,args',[]) -> let opts = foldr ($) defaultOptions fs in (opts,args')
  (_,_,es)      -> error (unlines [unwords es, usage])

spectrofy :: (Options, [String]) -> IO ()
spectrofy (Options{..},args)
  | showHelp  = putStrLn usage
  | otherwise = case args of
    ifile:ofile:_ -> do
      img <- R.readImageFromBMP ifile
      case img of
        Left err   -> error $ show err
        Right img' ->
          let fmt = R.wav16 {R.samplerate = samplingRate}
          in  R.writeSF ofile fmt (R.force $ guts samplingRate numDelta img')
    _             -> error usage


-----------------------------------------------------------------------------
-- Guts

guts :: Int -> Int -> Array DIM3 Word8 -> Array DIM2 Double
guts rate delta = squash . sumSins . toSins rate . to3D delta . rgbamp
{-# INLINE guts #-}

rgbamp :: Array DIM3 Word8 -> Array DIM2 Double
rgbamp arr = R.traverse arr f g where
  f (Z:.i:.j:._) = Z:.i:.j
  g h ix = 10 ** ((red*0.21 + green*0.71 + blue*0.07) / (255*3)) - 1 where
    red = fromIntegral $ h (ix:.0)
    green = fromIntegral $ h (ix:.1)
    blue = fromIntegral $ h (ix:.2)
{-# INLINE rgbamp #-}

to3D :: Int -> Array DIM2 Double -> Array DIM3 Double
to3D n = R.extend (Z :. All :. All :. n)
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
