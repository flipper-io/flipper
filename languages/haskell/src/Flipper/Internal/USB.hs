{-|
Module      : Flipper.Internal.USB
Description : Internal USB Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.USB (
    configure
  ) where

import Data.Word

import Flipper.Internal.Utils

configure :: IO Bool
configure = retSuc <$> c_usb_configure

foreign import ccall safe "flipper/usb/usb.h usb_configure"
    c_usb_configure :: IO Word32
