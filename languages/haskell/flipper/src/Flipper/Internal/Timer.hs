{-|
Module      : Flipper.Internal.Timer
Description : Internal Timer Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.Timer (
    configure
  ) where

import Data.Word

import Flipper.Internal.Utils

configure :: IO Bool
configure = retSuc <$> c_timer_configure

foreign import ccall safe "flipper/timer/timer.h timer_configure"
    c_timer_configure :: IO Word32
