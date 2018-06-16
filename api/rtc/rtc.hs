{-|
Module      : Flipper.RTC
Description : RTC Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.RTC (
    configure
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.RTC as I

-- | Configure the RTC.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure
