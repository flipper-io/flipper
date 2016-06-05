{-|
Module      : Flipper.Config
Description : Configuration Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides an interface to a Flipper's devices low-level configuration
key-value store.
-}

module Flipper.Config (
    read
  , write
  ) where

import Prelude hiding (read)

import Data.Word

import Flipper.MonadFlipper

import qualified Flipper.Internal.Config as I

-- | Read a configuration key.
read :: MonadFlipper m => Word8 -> m Word16
read = bracketIO . I.read

-- | Write a configuration key.
write :: MonadFlipper m => Word8 -> Word16 -> m ()
write = (bracketIO .) . I.write
