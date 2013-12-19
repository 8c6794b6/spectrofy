{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6, 2011-2013
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Convert image to sound file.

Spectrogram of result sound file resembles to input image.

-}
module Main (main) where

import System.Environment (getArgs)
import Spectrofy.CLI

main :: IO ()
main = spectrofy =<< getArgs
