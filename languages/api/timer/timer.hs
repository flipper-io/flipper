{-|
Module      : Flipper.Timer
Description : Timer Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Timer (
    configure
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.Timer as I

-- | Configure the timers.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure
