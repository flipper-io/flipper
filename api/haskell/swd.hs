{-|
Module      : Flipper.SWD
Description : Software Debugger Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.SWD (
    configure
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.SWD as I

-- | Configure the SWD.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure
