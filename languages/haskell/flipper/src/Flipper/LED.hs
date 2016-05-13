{-|
Module      : Flipper.LED
Description : Control Flipper's Status LED.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.LED (
    I.RGB(..)
  , setRGB
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.LED as I

setRGB :: MonadFlipper m => I.RGB -> m ()
setRGB = bracketIO . I.setRGB
