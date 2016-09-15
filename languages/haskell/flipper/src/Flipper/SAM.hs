{-|
Module      : Flipper.SAM
Description : SAM Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides an interface for controlling the AT91SAM4S, the principal
CPU on the Flipper device.
-}

module Flipper.SAM (
    setPower
  , reset
  , loadDFU
  , format
  , suspend
  , engage
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.SAM as I

-- | Set the device power state. 'True' turns the SAM on , 'False' turns the
--   SAM off.
setPower :: MonadFlipper m => Bool -> m ()
setPower = bracketIO . I.setPower

-- | Reset the SAM.
reset :: MonadFlipper m => m ()
reset = bracketIO I.reset

-- | Put the SAM into Device Firmware Update mode.
loadDFU :: MonadFlipper m => m ()
loadDFU = bracketIO I.loadDFU

-- | Format the SAM program ROM.
format :: MonadFlipper m => m ()
format = bracketIO I.format

-- | Turn off the SAM.
suspend :: MonadFlipper m => m ()
suspend = bracketIO I.suspend

-- | Turn on the SAM.
engage :: MonadFlipper m => m ()
engage = bracketIO I.engage
