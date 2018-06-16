{-|
Module      : Flipper.Internal.CPU
Description : Internal SAM4S CPU Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.CPU (
    configure
  , reset
  , halt
  , power
  , dfu
  ) where

import Data.Word

import Foreign.Marshal.Utils

import Flipper.Internal.Utils

configure :: IO Bool
configure = retSuc <$> c_cpu_configure

reset :: IO ()
reset = c_cpu_reset

halt :: IO ()
halt = c_cpu_halt

power :: Bool -> IO ()
power = c_cpu_power . fromBool

dfu :: IO ()
dfu = c_cpu_dfu

foreign import ccall safe "flipper/cpu/cpu.h cpu_configure"
    c_cpu_configure :: IO Word32

foreign import ccall safe "flipper/cpu/cpu.h cpu_reset"
    c_cpu_reset :: IO ()

foreign import ccall safe "flipper/cpu/cpu.h cpu_halt"
    c_cpu_halt :: IO ()

foreign import ccall safe "flipper/cpu/cpu.h cpu_power"
    c_cpu_power :: Word8 -> IO ()

foreign import ccall safe "flipper/cpu/cpu.h cpu_dfu"
    c_cpu_dfu :: IO ()
