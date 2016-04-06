module Flipper.Internal.Config where

import Data.Word

write :: Word8 -> Word16 -> IO ()
write = c_config_write

read :: Word8 -> IO Word16
read = c_config_read

foreign import ccall safe "flipper/config.h config_write"
    c_config_write :: Word8 -> Word16 -> IO ()

foreign import ccall safe "flipper/config.h config_read"
    c_config_read :: Word8 -> IO Word16
