{-|
Module      : Flipper.Internal.CPU
Description : Internal SAM43 CPU Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.CPU (
    reset
  , halt
  , power
  , dfu
  ) where

import Data.Word

import Foreign.Marshal.Utils

reset :: IO ()
reset = c_cpu_reset

halt :: IO ()
halt = c_cpu_halt

power :: Bool -> IO ()
power = c_cpu_power . fromBool

dfu :: IO ()
dfu = c_cpu_dfu

foreign import ccall safe "flipper/cpu.h cpu_reset"
    c_cpu_reset :: IO ()

foreign import ccall safe "flipper/cpu.h cpu_halt"
    c_cpu_halt :: IO ()

foreign import ccall safe "flipper/cpu.h cpu_power"
    c_cpu_power :: Word8 -> IO ()

foreign import ccall safe "flipper/cpu.h cpu_dfu"
    c_cpu_dfu :: IO ()
