{-|
Module      : Flipper.Internal.LED
Description : Internal LED Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

{-# LANGUAGE DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           #-}

module Flipper.Internal.LED (
    RGB(..)
  , configure
  , setRGB
  ) where

import Control.DeepSeq

import Data.Data

import Data.Word

import Flipper.Internal.Utils

import GHC.Generics

-- | RGB value.
--   TODO: Provide an actual algebra and vector space for this.
data RGB = RGB {
    red   :: Word8 -- ^ Red value.
  , green :: Word8 -- ^ Green value.
  , blue  :: Word8 -- ^ Blue value.
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Data
             , Typeable
             , Generic
             , NFData
             )

configure :: IO Bool
configure = retSuc <$> c_led_configure

setRGB :: RGB -> IO ()
setRGB (RGB r g b) = c_led_rgb r g b

foreign import ccall safe "flipper/led/led.h led_configure"
    c_led_configure :: IO Word32

foreign import ccall safe "flipper/led/led.h led_rgb"
    c_led_rgb :: Word8 -> Word8 -> Word8 -> IO ()
