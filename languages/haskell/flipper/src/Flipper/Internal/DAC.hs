{-|
Module      : Flipper.Internal.DAC
Description : Internal DAC Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.DAC (
    configure
  ) where

import Data.Word

import Flipper.Internal.Utils

configure :: IO Bool
configure = retSuc <$> c_dac_configure

foreign import ccall safe "flipper/dac/dac.h dac_configure"
    c_dac_configure :: IO Word32
