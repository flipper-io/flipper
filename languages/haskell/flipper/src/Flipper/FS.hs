{-|
Module      : Flipper.FS
Description : Flipper File System
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.FS (
    I.FSHandle()
  , format
  , create
  , remove
  , rename
  , withGet
  , withPut
  , getHandle
  ) where

import Data.Word

import Flipper.Buffer
import Flipper.Bufferable
import Flipper.MonadFlipper
import Flipper.Put

import qualified Flipper.Internal.FS as I

format :: MonadFlipper m => m ()
format = bracketIO I.format

create :: MonadFlipper m => String -> Buffer -> m ()
create = (bracketIO .) . I.create

remove :: MonadFlipper m => String -> m ()
remove = bracketIO . remove

rename :: MonadFlipper m => String -> String -> m ()
rename = (bracketIO .) . I.rename

withGet :: MonadFlipper m => String -> (IO Word8 -> IO a) -> m a
withGet = (bracketIO .) . I.withGet

withPut :: MonadFlipper m => String -> ((Word8 -> IO ()) -> IO a) -> m a
withPut = (bracketIO .) . I.withPut

getHandle :: MonadFlipper m => String -> m I.FSHandle
getHandle = bracketIO . I.getHandle
