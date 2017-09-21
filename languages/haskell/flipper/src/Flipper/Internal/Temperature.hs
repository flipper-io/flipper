{-|
Module      : Flipper.Internal.Temperature
Description : Internal Temperature Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.Temperature (
    configure
  ) where

import Data.Word

import Flipper.Internal.Utils

configure :: IO Bool
configure = retSuc <$> c_temp_configure

foreign import ccall safe "flipper/temp/temp.h temp_configure"
    c_temp_configure :: IO Word32
