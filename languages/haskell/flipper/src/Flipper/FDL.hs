{-|
Module      : Flipper.FDL
Description : Flipper Dynamic Loader Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.FDL (
    I.FDLKey()
  , I.FDLAddress()
  , load
  , launch
  , resolve
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.FDL as I

load :: MonadFlipper m => I.FDLKey -> m I.FDLAddress
load = bracketIO . I.load

launch :: MonadFlipper m => I.FDLKey -> m ()
launch = bracketIO . I.launch

resolve :: MonadFlipper m => I.FDLKey -> I.FDLAddress -> m ()
resolve = (bracketIO .) . I.resolve
