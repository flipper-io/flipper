{-|
Module      : Flipper.DAC
Description : Digital to Analog Converter
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.DAC (
    configure
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.DAC as I

-- | Configure the DAC.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure
