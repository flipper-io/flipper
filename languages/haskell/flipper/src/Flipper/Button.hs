{-|
Module      : Flipper.Button
Description : Button Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides an interface to Flipper's on-board button.
-}

module Flipper.Button (
    configure
  , read
  ) where

import Prelude hiding (read)

import Flipper.MonadFlipper

import qualified Flipper.Internal.Button as I

-- | Configure the button.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure

-- | Read the button state.
read :: MonadFlipper m => m Bool
read = bracketIO I.read
