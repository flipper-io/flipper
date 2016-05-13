{-|
Module      : Flipper.SPI
Description : SPI Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides an interface for sending/receiving data over the Flipper
device's SPI bus.
-}

module Flipper.SPI (
    -- * SPI Control
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

import qualified Flipper.Internal.SPI as I

-- | Enable the SPI bus.
enable :: MonadFlipper m => m ()
enable = bracketIO I.enable

-- | Disable the SPI bus.
disable :: MonadFlipper m => m ()
disable = bracketIO I.disable

-- Send any 'Bufferable' data over the SPI bus.
push :: (Bufferable b, MonadFlipper m) => b -> m ()
push = bracketIO . I.push . runPut . put

-- Receive any 'Bufferable' data over the SPI bus.
pull :: (Bufferable b, MonadFlipper m) => m (Either String b)
pull = runGetWith get (bracketIO . I.pull) emptyBuffer
