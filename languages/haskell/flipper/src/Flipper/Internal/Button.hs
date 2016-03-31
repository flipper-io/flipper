module Flipper.Internal.Button where

import Data.Word

import Flipper.Internal.Utils

read :: IO Bool
read = retSuc <$> c_button_read

foreign import ccall safe "flipper/button/button.h button_read"
    c_button_read :: IO Word8
