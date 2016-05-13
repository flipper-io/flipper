{-|
Module      : Flipper
Description : Flipper Device Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper (
    I.Endpoint(..)
  , select
  , attach
  , detach
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.Flipper as I

select :: MonadFlipper m => String -> m Bool
select = bracketIO . I.select

attach :: MonadFlipper m => I.Endpoint -> m Bool
attach = bracketIO . I.attach

detach :: MonadFlipper m => String -> m Bool
detach = bracketIO . I.detach
