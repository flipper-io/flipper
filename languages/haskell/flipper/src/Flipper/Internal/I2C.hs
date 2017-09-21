{-|
Module      : Flipper.Internal.I2C
Description : Internal I2C Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.I2C (
    configure
  ) where

import Data.Word

import Flipper.Internal.Utils

configure :: IO Bool
configure = retSuc <$> c_i2c_configure

foreign import ccall safe "flipper/i2c/i2c.h i2c_configure"
    c_i2c_configure :: IO Word32
