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

-- | The USART units available on the device.
data USART = USART0 -- ^ What pins provide USART0?
           | USART1 -- ^ What pins provide USART1?
           | DBGU   -- ^ What pins provide DBGU?
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

-- | Send a byte over a USART bus.
put :: MonadFlipper m => USART -> Word8 -> m ()
put USART0 = bracketIO . I.usart0Put
put USART1 = bracketIO . I.usart1Put
put DBGU   = bracketIO . I.dbguPut

-- | Receive a byte over a USART bus.
get :: MonadFlipper m => USART -> m Word8
get USART0 = bracketIO I.usart0Get
get USART1 = bracketIO I.usart1Get
get DBGU   = bracketIO I.dbguGet

-- | Send any 'Bufferable' data over a USART bus.
push :: (B.Bufferable b, MonadFlipper m) => USART -> b -> m ()
push USART0 = bracketIO . I.usart0Push . runPut . B.put
push USART1 = bracketIO . I.usart1Push . runPut . B.put
push DBGU   = bracketIO . I.dbguPush . runPut . B.put

-- | Receive any 'Bufferable' data over a USART bus.
pull :: (B.Bufferable b, MonadFlipper m) => USART -> m (Either String b)
pull USART0 = runGetWith B.get (bracketIO . I.usart0Pull) emptyBuffer
pull USART1 = runGetWith B.get (bracketIO . I.usart1Pull) emptyBuffer
pull DBGU   = runGetWith B.get (bracketIO . I.dbguPull) emptyBuffer
