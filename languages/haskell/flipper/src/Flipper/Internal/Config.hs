{-|
Module      : Flipper.Internal.Config
Description : Internal Config Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.Config (
    write
  , read
  ) where

import Prelude hiding (read)

import Data.Word

write :: Word8 -> Word16 -> IO ()
write = c_config_write

read :: Word8 -> IO Word16
read = c_config_read

foreign import ccall safe "flipper/config.h config_write"
    c_config_write :: Word8 -> Word16 -> IO ()

foreign import ccall safe "flipper/config.h config_read"
    c_config_read :: Word8 -> IO Word16
