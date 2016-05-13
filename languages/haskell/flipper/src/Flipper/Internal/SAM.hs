{-|
Module      : Flipper.Internal.SAM
Description : Internal SAM Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.SAM (
    setPower
  , reset
  , loadDFU
  , format
  , suspend
  , engage
  ) where

import Data.Word

import Foreign.Marshal.Utils

setPower :: Bool -> IO ()
setPower = c_sam_set_power . fromBool

reset :: IO ()
reset = c_sam_reset

loadDFU :: IO ()
loadDFU = c_sam_load_dfu

format :: IO ()
format = c_sam_format

suspend :: IO ()
suspend = c_sam_suspend

engage :: IO ()
engage = c_sam_engage

foreign import ccall safe "flipper/sam.h sam_set_power"
    c_sam_set_power :: Word8 -> IO ()

foreign import ccall safe "flipper/sam.h sam_reset"
    c_sam_reset :: IO ()

foreign import ccall safe "flipper/sam.h sam_load_dfu"
    c_sam_load_dfu :: IO ()

foreign import ccall safe "flipper/sam.h sam_format"
    c_sam_format :: IO ()

foreign import ccall safe "flipper/sam.h sam_suspend"
    c_sam_suspend :: IO ()

foreign import ccall safe "flipper/sam.h sam_engage"
    c_sam_engage :: IO ()
