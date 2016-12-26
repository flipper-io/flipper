{-|
Module      : Flipper.USB
Description : USB Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides an interface for sending data to and receiving data from a
Flipper device over USB.
-}

module Flipper.USB (
    configure
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.USB as I

-- | Configure the USB.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure
