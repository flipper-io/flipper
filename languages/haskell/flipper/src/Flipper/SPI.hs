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
    configure
  , enable
  , disable
    -- * Sending/Receiving Data
  , put
  , get
  , push
  , pull) where

import Data.Word

import Flipper.Buffer
import Flipper.Get
import Flipper.Put
import Flipper.MonadFlipper

import qualified Flipper.Bufferable   as B
import qualified Flipper.Internal.SPI as I

-- | Configure the SPI.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure

-- | Enable the SPI bus.
enable :: MonadFlipper m => m ()
enable = bracketIO I.enable

-- | Disable the SPI bus.
disable :: MonadFlipper m => m ()
disable = bracketIO I.disable

-- | Send a byte over the SPI bus.
put :: MonadFlipper m => Word8 -> m ()
put = bracketIO . I.put

-- | Receive a byte over the SPI bus.
get :: MonadFlipper m => m Word8
get = bracketIO I.get

-- | Send any 'Bufferable' data over the SPI bus.
push :: (B.Bufferable b, MonadFlipper m) => b -> m ()
push = bracketIO . I.push . runPut . B.put

-- | Receive any 'Bufferable' data over the SPI bus.
pull :: (B.Bufferable b, MonadFlipper m) => m (Either String b)
pull = runGetWith B.get (bracketIO . I.pull) emptyBuffer
