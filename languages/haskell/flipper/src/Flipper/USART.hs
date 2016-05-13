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
    -- * Bus Selection
    USART(..)
    -- * Bus Control
  , enable
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

import qualified Flipper.Internal.USART as I

-- | The USART units available on the device.
data USART = USART0
           | USART1
           | DBGU
           deriving (Eq, Ord, Show)

-- | Enable a USART bus.
enable :: MonadFlipper m => USART -> m ()
enable USART0 = bracketIO I.usart0Enable
enable USART1 = bracketIO I.usart1Enable
enable DBGU   = bracketIO I.dbguEnable

-- | Disable a USART bus.
disable :: MonadFlipper m => USART -> m ()
disable USART0 = bracketIO I.usart0Disable
disable USART1 = bracketIO I.usart1Disable
disable DBGU   = bracketIO I.dbguDisable

-- | Send any 'Bufferable' data over a USART bus.
push :: (Bufferable b, MonadFlipper m) => USART -> b -> m ()
push USART0 = bracketIO . I.usart0Push . runPut . put
push USART1 = bracketIO . I.usart1Push . runPut . put
push DBGU   = bracketIO . I.dbguPush . runPut . put

-- | Receive any 'Bufferable' data over a USART bus.
pull :: (Bufferable b, MonadFlipper m) => USART -> m (Either String b)
pull USART0 = runGetWith get (bracketIO . I.usart0Pull) emptyBuffer
pull USART1 = runGetWith get (bracketIO . I.usart1Pull) emptyBuffer
pull DBGU   = runGetWith get (bracketIO . I.dbguPull) emptyBuffer
