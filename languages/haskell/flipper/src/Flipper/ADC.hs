{-|
Module      : Flipper.ADC
Description : Analog to Digital Converter
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.ADC (
    configure
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.ADC as I

-- | Configure the ADC.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure
