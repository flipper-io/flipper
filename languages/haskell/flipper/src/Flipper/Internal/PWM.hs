{-|
Module      : Flipper.Internal.PWM
Description : Internal PWM Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.PWM (
    configure
  ) where

import Data.Word

import Flipper.Internal.Utils

configure :: IO Bool
configure = retSuc <$> c_pwm_configure

foreign import ccall safe "flipper/pwm/pwm.h pwm_configure"
    c_pwm_configure :: IO Word32
