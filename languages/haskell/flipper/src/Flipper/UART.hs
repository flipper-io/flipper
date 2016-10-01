{-|
Module      : Flipper.UART
Description : UART Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides an interface for sending and receiving 'Bufferable' data
over Flipper's UARTs.
-}

module Flipper.UART (
    -- * Bus Control
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

import qualified Flipper.Bufferable     as B
import qualified Flipper.Internal.UART as I

-- | Enable the UART bus.
enable :: MonadFlipper m => m ()
enable = bracketIO I.uartEnable

-- | Disable the UART bus.
disable :: MonadFlipper m => m ()
disable = bracketIO I.uartDisable

-- | Send a byte over the UART bus.
put :: MonadFlipper m => Word8 -> m ()
put = bracketIO . I.uartPut

-- | Receive a byte over the UART bus.
get :: MonadFlipper m => m Word8
get = bracketIO I.uartGet

-- | Send any 'Bufferable' data over the UART bus.
push :: (B.Bufferable b, MonadFlipper m) => b -> m ()
push = bracketIO . I.uartPush . runPut . B.put

-- | Receive any 'Bufferable' data over the UART bus.
pull :: (B.Bufferable b, MonadFlipper m) => m (Either String b)
pull = runGetWith B.get (bracketIO . I.uartPull) emptyBuffer
