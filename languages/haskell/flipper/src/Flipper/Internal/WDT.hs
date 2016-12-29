{-|
Module      : Flipper.Internal.WDT
Description : Internal Watchdog Timer Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.WDT (
    configure
  , fire
  ) where

import Data.Word

import Flipper.Internal.Utils

configure :: IO Bool
configure = retSuc <$> c_wdt_configure

fire :: IO ()
fire = c_wdt_fire

foreign import ccall safe "flipper/wdt/wdt.h wdt_configure"
    c_wdt_configure :: IO Word32

foreign import ccall safe "flipper/wdt/wdt.h wdt_fire"
    c_wdt_fire :: IO ()
