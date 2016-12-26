{-|
Module      : Flipper.Internal.RTC
Description : Internal RTC Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.RTC (
    configure
  ) where

import Data.Word

import Flipper.Internal.Utils

configure :: IO Bool
configure = retSuc <$> c_rtc_configure

foreign import ccall safe "flipper/rtc/rtc.h rtc_configure"
    c_rtc_configure :: IO Word32
