{-|
Module      : Flipper.USART
Description : USART Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides an interface for sending and receiving 'Bufferable' data
over Flipper's USARTs.
-}

module Flipper.USART (
    -- * Bus Control
    enable
  , disable
    -- * Sending/Receiving Data
  , ready
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
import qualified Flipper.Internal.USART as I

-- | Enable the UART bus.
enable :: MonadFlipper m => m ()
enable = bracketIO I.usartEnable

-- | Disable the UART bus.
disable :: MonadFlipper m => m ()
disable = bracketIO I.usartDisable

-- | Check if there is data in the USART read buffer.
ready :: MonadFlipper m => m Bool
ready = bracketIO I.usartReady

-- | Send a byte over the UART bus.
put :: MonadFlipper m => Word8 -> m ()
put = bracketIO . I.usartPut

-- | Receive a byte over the UART bus.
get :: MonadFlipper m => m Word8
get = bracketIO I.usartGet

-- | Send any 'Bufferable' data over the UART bus.
push :: (B.Bufferable b, MonadFlipper m) => b -> m ()
push = bracketIO . I.usartPush . runPut . B.put

-- | Receive any 'Bufferable' data over the UART bus.
pull :: (B.Bufferable b, MonadFlipper m) => m (Either String b)
pull = runGetWith B.get (bracketIO . I.usartPull) emptyBuffer
