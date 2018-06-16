{-|
Module      : Flipper.CPU
Description : SAM4S CPU Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides an interface for controlling the AT91SAM4S, the principal
CPU on the Flipper device.
-}

module Flipper.CPU (
    configure
  , reset
  , halt
  , power
  , dfu
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.CPU as I

-- | Configure the CPU.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure

-- | Reset the SAM.
reset :: MonadFlipper m => m ()
reset = bracketIO I.reset

-- | Halt the CPU.
halt :: MonadFlipper m => m ()
halt = bracketIO I.reset

-- | Set the device power state. 'True' turns the SAM on , 'False' turns the
--   SAM off.
power :: MonadFlipper m => Bool -> m ()
power = bracketIO . I.power

-- | Put the SAM into Device Firmware Update mode.
dfu :: MonadFlipper m => m ()
dfu = bracketIO I.dfu
