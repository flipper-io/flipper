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
  , put
  , get
  , push
  , pull
  ) where

import Data.Word

import Flipper.Buffer
import Flipper.Get
import Flipper.Put
import Flipper.MonadFlipper

import qualified Flipper.Bufferable   as B
import qualified Flipper.Internal.USB as I

-- | Enable USB.
enable :: MonadFlipper m => m ()
enable = bracketIO I.enable

-- | Disable USB.
disable :: MonadFlipper m => m ()
disable = bracketIO I.disable

-- | Send a byte over USB.
put :: MonadFlipper m => Word8 -> m ()
put = bracketIO . I.put

-- | Receive a byte over USB.
get :: MonadFlipper m => m Word8
get = bracketIO I.get

-- | Send any 'Bufferable' data to the device over USB.
push :: (B.Bufferable b, MonadFlipper m) => b -> m ()
push = bracketIO . I.push . runPut . B.put

-- | Receive any 'Bufferable' data from the device over USB.
pull :: (B.Bufferable b, MonadFlipper m) => m (Either String b)
pull = runGetWith B.get (bracketIO . I.pull) emptyBuffer
