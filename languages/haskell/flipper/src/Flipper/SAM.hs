{-|
Module      : Flipper.SAM
Description : SAM Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.SAM (
    setPower
  , reset
  , loadDFU
  , format
  , suspend
  , engage
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.SAM as I

setPower :: MonadFlipper m => Bool -> m ()
setPower = bracketIO . I.setPower

reset :: MonadFlipper m => m ()
reset = bracketIO I.reset

loadDFU :: MonadFlipper m => m ()
loadDFU = bracketIO I.loadDFU

format :: MonadFlipper m => m ()
format = bracketIO I.format

suspend :: MonadFlipper m => m ()
suspend = bracketIO I.suspend

engage :: MonadFlipper m => m ()
engage = bracketIO I.engage
