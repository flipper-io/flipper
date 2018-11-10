{-|
Module      : Flipper.GPIO
Description : GPIO Pin Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides an interface to a Flipper device's GPIO pins.
-}

module Flipper.GPIO (
    -- * IO Pins
    I.DigitalPin(..)
  , I.AnalogPin(..)
  , I.Direction(..)
    -- * Configuration
  , configure
    -- * Reading/Writing Pin State
  , digitalDirection
  , digitalRead
  , digitalWrite
  , analogDirection
  , analogRead
  , analogWrite
  ) where

import Data.Word

import Flipper.MonadFlipper

import qualified Flipper.Internal.GPIO as I

-- | Configure the GPIO.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure

-- | Set the IO direction of a digital pin.
digitalDirection :: MonadFlipper m => I.DigitalPin -> I.Direction -> m ()
digitalDirection = (bracketIO .) . I.digitalDirection

-- | Read a 'Bool' from a digital pin.
digitalRead :: MonadFlipper m => I.DigitalPin -> m Bool
digitalRead = bracketIO . I.digitalRead

-- | Write a 'Bool' to a digital pin.
digitalWrite :: MonadFlipper m => I.DigitalPin -> Bool -> m ()
digitalWrite = (bracketIO .) . I.digitalWrite

-- | Set the IO direction of a digital pin.
analogDirection :: MonadFlipper m => I.AnalogPin -> I.Direction -> m ()
analogDirection = (bracketIO .) . I.analogDirection

-- | Make this interface more clever.
analogRead :: MonadFlipper m => I.AnalogPin -> m Word16
analogRead = bracketIO . I.analogRead

-- | Make this interface more clever.
analogWrite :: MonadFlipper m => I.AnalogPin -> Word16 -> m ()
analogWrite = (bracketIO .) . I.analogWrite
