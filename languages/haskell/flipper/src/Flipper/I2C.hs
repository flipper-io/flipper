{-|
Module      : Flipper.I2C
Description : I2C Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.I2C (
    configure
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.I2C as I

-- | Configure the I2C.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure
