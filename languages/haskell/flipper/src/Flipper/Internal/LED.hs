{-|
Module      : Flipper.Internal.LED
Description : Internal LED Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.LED (
    RGB(..)
  , setRGB
  ) where

import Data.Word

-- | RGB value.
data RGB = RGB {
    red   :: Word8 -- ^ Red value.
  , green :: Word8 -- ^ Green value.
  , blue  :: Word8 -- ^ Blue value.
  } deriving (Eq, Ord, Show)

setRGB :: RGB -> IO ()
setRGB (RGB r g b) = c_flipper_set_rgb r g b

foreign import ccall safe "flipper/led/led.h led_set_rgb"
    c_flipper_set_rgb :: Word8 -> Word8 -> Word8 -> IO ()
