{-|
Module      : Flipper.NVM
Description : Non Volatile Memory Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides an interface to a Flipper device's non-volatile memory.
This memory may be used as a simple persistent file system in which any
'Bufferable' data may be stored.

This module, like all others in this package, is intended to be imported
@qualified@, e.g.

> import qualified Flipper.NVM as NVM
-}

module Flipper.NVM (
    -- * Chip Control
    enable
  , disable
  , reset
    -- * Allocating Space
  , alloc
    -- * Reading and Writing Data
  , push
  , pushHandle
  , pull
    -- * Deleting Data
  , free
  , format
  ) where

import Data.Word

import Flipper.Buffer
import Flipper.Bufferable
import Flipper.FS
import Flipper.Get
import Flipper.Put
import Flipper.MonadFlipper

import qualified Flipper.Internal.NVM as I

-- | Enable the AT45 flash chip. The chip must be enabled before any other
--   operations may be performed.
enable :: MonadFlipper m => m ()
enable = bracketIO I.enable

-- | Disable the AT45 flash chip.
disable :: MonadFlipper m => m ()
disable = bracketIO I.disable

-- | Reset the AT45 flash chip.
reset :: MonadFlipper m => m ()
reset = bracketIO I.reset

-- | Allocate a contiguous block of the provided size.
alloc :: MonadFlipper m =>  Word32 -> m FSHandle
alloc = bracketIO . I.alloc

-- | Free the allocated block associated with an FSHandle.
free :: MonadFlipper m => FSHandle -> m ()
free = bracketIO . I.free

-- | Format the AT45 flash chip, removing all stored data.
format :: MonadFlipper m => m ()
format = bracketIO I.format

-- | Push data to the chip. This function allocates a block of storage for the
--   data that may later be deallocated with 'free'.
push :: (Bufferable b, MonadFlipper m) => b -> m FSHandle
push b = do
    h <- alloc (fromIntegral (sizePut (put b)))
    pushHandle b h
    return h

-- | Push data to an existing 'FSHandle'. Beware, adjacent blocks may be
--   overwritten if the block allocated for the 'FSHandle' isn't large enough to
--   accomodate the write.
pushHandle :: (Bufferable b, MonadFlipper m) => b -> FSHandle -> m ()
pushHandle b = bracketIO  . I.push (runPut (put b))

-- | Read data from an 'FSHandle'.
pull :: (Bufferable b, MonadFlipper m) => FSHandle -> m (Either String b)
pull h = (bracketIO (I.pullAdvance h)) >>= (\p -> runGetWith get (bracketIO . p) emptyBuffer)
