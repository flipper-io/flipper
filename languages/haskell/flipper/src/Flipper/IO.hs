{-|
Module      : Flipper.IO
Description : IO Pin Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.IO (
    I.DigitalPin(..)
  , I.AnalogPin(..)
  , I.Direction(..)
  , digitalDirection
  , digitalRead
  , digitalWrite
  , analogDirection
  , analogRead
  , analogWrite
  ) where

import Data.Word

import Flipper.MonadFlipper

import qualified Flipper.Internal.IO as I

digitalDirection :: MonadFlipper m => I.DigitalPin -> I.Direction -> m ()
digitalDirection = (bracketIO .) . I.digitalDirection

digitalRead :: MonadFlipper m => I.DigitalPin -> m Bool
digitalRead = bracketIO . I.digitalRead

digitalWrite :: MonadFlipper m => I.DigitalPin -> Bool -> m ()
digitalWrite = (bracketIO .) . I.digitalWrite

analogDirection :: MonadFlipper m => I.AnalogPin -> I.Direction -> m ()
analogDirection = (bracketIO .) . I.analogDirection

-- | Make this interface more clever.
analogRead :: MonadFlipper m => I.AnalogPin -> m Word16
analogRead = bracketIO . I.analogRead

-- | make this interface more clever.
analogWrite :: MonadFlipper m => I.AnalogPin -> Word16 -> m ()
analogWrite = (bracketIO .) . I.analogWrite
