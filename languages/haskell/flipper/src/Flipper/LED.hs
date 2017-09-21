{-|
Module      : Flipper.LED
Description : Control Flipper's Status LED.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides an interface to a Flipper device's built in RGB LED. This
LED is ideal for displaying status information.
-}

module Flipper.LED (
    I.RGB(..)
  , configure
  , setRGB
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.LED as I

-- | Configure the LED.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure

-- | Set the RGB LED color.
setRGB :: MonadFlipper m => I.RGB -> m ()
setRGB = bracketIO . I.setRGB
