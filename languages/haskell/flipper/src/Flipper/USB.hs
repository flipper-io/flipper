{-|
Module      : Flipper.USB
Description : USB Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.USB (
    enable
  , disable
  , push
  , pull
  ) where

import Flipper.Buffer
import Flipper.Bufferable
import Flipper.Get
import Flipper.Put
import Flipper.MonadFlipper

import qualified Flipper.Internal.USB as I

enable :: MonadFlipper m => m ()
enable = bracketIO I.usbEnable

disable :: MonadFlipper m => m ()
disable = bracketIO I.usbDisable

push :: (Bufferable b, MonadFlipper m) => b -> m ()
push = bracketIO . I.usbPush . runPut . put

pull :: (Bufferable b, MonadFlipper m) => m (Either String b)
pull = runGetWith get (bracketIO . I.usbPull) emptyBuffer
