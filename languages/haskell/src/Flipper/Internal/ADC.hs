{-|
Module      : Flipper.Internal.ADC
Description : Internal Analog to Digital Converter Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.ADC (
    configure
  ) where

import Data.Word

import Flipper.Internal.Utils

configure :: IO Bool
configure = retSuc <$> c_adc_configure

foreign import ccall safe "flipper/adc/adc.h adc_configure"
    c_adc_configure :: IO Word32
