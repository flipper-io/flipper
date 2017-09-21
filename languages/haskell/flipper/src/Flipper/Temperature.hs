{-|
Module      : Flipper.Temperature
Description : Temperature Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Temperature (
    configure
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.Temperature as I

-- | Configure the thermal hardware.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure
