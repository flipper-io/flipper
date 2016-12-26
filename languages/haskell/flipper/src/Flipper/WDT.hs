{-|
Module      : Flipper.WDT
Description : Watchdog Timer
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.WDT (
    configure
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.WDT as I

-- | Configure the watchdog timer.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure
