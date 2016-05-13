{-|
Module      : Flipper.Config
Description : Configuration Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Config (
    read
  , write
  ) where

import Prelude hiding (read)

import Data.Word

import qualified Flipper.Internal.Config as I

read :: MonadFlipper m => Word8 -> m Word16
read = bracketIO . I.read

write :: MonadFlipper m => Word8 -> Word16 -> m ()
write = bracketIO . I.write
