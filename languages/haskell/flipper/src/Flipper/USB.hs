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
    -- * USB Control
    enable
  , disable
    -- * Sending/Receiving Data
  , push
  , pull
  ) where

import Flipper.Buffer
import Flipper.Bufferable
import Flipper.Get
import Flipper.Put
import Flipper.MonadFlipper

import qualified Flipper.Internal.USB as I

-- | Enable USB.
enable :: MonadFlipper m => m ()
enable = bracketIO I.usbEnable

-- | Disable USB.
disable :: MonadFlipper m => m ()
disable = bracketIO I.usbDisable

-- | Send any 'Bufferable' data to the device over USB.
push :: (Bufferable b, MonadFlipper m) => b -> m ()
push = bracketIO . I.usbPush . runPut . put

-- | Receive any 'Bufferable' data from the device over USB.
pull :: (Bufferable b, MonadFlipper m) => m (Either String b)
pull = runGetWith get (bracketIO . I.usbPull) emptyBuffer
