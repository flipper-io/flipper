{-|
Module      : Flipper.Button
Description : Button Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Button (
    read
  ) where

import Prelude hiding (read)

import Flipper.MonadFlipper

import qualified Flipper.Internal.Button as I

read :: MonadFlipper m => m Bool
read = bracketIO I.read
