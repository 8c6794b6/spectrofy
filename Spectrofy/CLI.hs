{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6, 2011-2013
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Entry point for command line interface.

-}
module Spectrofy.CLI (spectrofy) where

import System.Console.GetOpt
  (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.IO.BMP as R
import qualified Data.Array.Repa.IO.Sndfile as R
import qualified Data.Array.Repa.Repr.ForeignPtr as R

import qualified Spectrofy.Synth as S

-- | Receive list of flags, and do work.
--
-- Expecting the given list to hold flags and values.
-- e.g. returned value from System.Environment.getAargs action.
--
spectrofy :: [String] -> IO ()
spectrofy = work . parseOpts

-- --------------------------------------------------------------------------
-- Guts

parseOpts :: [String] -> ((Options, [String], [String]), Mode)
parseOpts args = case args of
  "sin":rest -> (f sinOpts rest, Sin)
  "fft":rest -> (f fftOpts rest, FFT)
  _          -> ((defaultOptions, [], []), Sin)
  where
    f o as = case getOpt Permute o as of
      (os, ns, es) -> (foldr ($) defaultOptions os, ns, es)

work :: ((Options, [String], [String]), Mode) -> IO ()
work ((o, args, errs),mode)
  | not (null errs) = putStr $ unlines (errs++[usage])
  | showHelp o      = putStr usage
  | otherwise       = case args of
    ifile:ofile:_ -> do
      let sr = samplingRate o
          write :: R.Array R.F (R.DIM2) Double -> IO ()
          write = R.writeSF ofile (R.wav16 {R.samplerate = sr})
      img <- (fmap R.delay) `fmap` R.readImageFromBMP ifile
      case img of
        Left err   -> error $ show err
        Right img' -> case mode of
          Sin -> write =<< R.computeP (S.sinsyn sr (numDelta o) img')
          FFT -> write =<< R.computeP (S.fftsyn (fftSize o) img')
    _ -> putStr usage

data Mode = Sin | FFT deriving (Eq, Show)

data Options = Options
  { showHelp :: Bool
  , samplingRate :: Int
  , numDelta :: Int
  , isTest :: Bool
  , fftSize :: Int
  } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
  { showHelp = False
  , samplingRate = 48000
  , numDelta = 4800
  , isTest = False
  , fftSize = 1024 }

rateOpt :: OptDescr (Options -> Options)
rateOpt =
  Option ['r'] ["rate"]
  (ReqArg (\v o -> o {samplingRate = read v}) "INT")
  "Sampling rate (default 48000)"

helpOpt :: OptDescr (Options -> Options)
helpOpt =
  Option ['h'] ["help"]
  (NoArg (\o -> o {showHelp = True}))
  "Show this help"

sinOpts :: [OptDescr (Options -> Options)]
sinOpts =
  [ Option ['d'] ["delta"]
    (ReqArg (\v flg -> flg {numDelta = read v}) "INT")
    "Number of delta samples (default 4800)"
  , rateOpt
  , helpOpt ]

fftOpts :: [OptDescr (Options -> Options)]
fftOpts =
  [ Option ['f'] ["fsize"]
    (ReqArg (\v flg -> flg {fftSize = read v}) "INT")
    "FFT size (default 1024)"
  , rateOpt
  , helpOpt ]

usage :: String
usage =
  unlines
    [ header
    , usageInfo "sin" sinOpts
    , usageInfo "fft" fftOpts
    , example ]
  where
    header =
      "Usage: spectrofy MODE [OPTIONS] INFILE OUTFILE\n\n\
      \Modes:\n"
    example =
      "Examples:\n\
       \\n\
       \  spectrofy sin -r 44100 -d 4410 in.bmp out.wav\n\
       \  spectrofy fft -r 48000 in.bmp out.wav\n"
