{-|
Module      : Flipper.FMR
Description : Flipper Message Runtime Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.FMR (
    I.FMRModule()
  , bind
  , invoke
  ) where

import Data.Word

import Flipper.MonadFlipper

import qualified Flipper.Internal.FMR as I

bind :: MonadFlipper m => String -> m I.FMRModule
bind = bracketIO . I.bind

invoke :: MonadFlipper m => I.FMRModule -> Word8 -> [Word32] -> m Word32
invoke = ((bracketIO .) .) . I.invoke
