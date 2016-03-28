module Flipper.Internal.Button where

import Data.Word

import Foreign.Marshal.Utils

read :: IO Bool
read = (not . toBool) <$> c_button_read

foreign import ccall safe "flipper/button/button.h button_read"
    c_button_read :: IO Word8
