{-|
Module      : Flipper.PWM
Description : PWM Module Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.PWM (
    configure
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.PWM as I

-- | Configure the PWM.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure
