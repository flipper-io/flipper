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
    configure
  , create
  , delete
  , size
  , open
  , push
  , pull
  , close
  , format
  ) where

import Data.Word

import Flipper.Buffer
import Flipper.Bufferable
import Flipper.Get
import Flipper.MonadFlipper
import Flipper.Put

import qualified Flipper.Internal.FS as I

-- | Configure the file system.
configure :: MonadFlipper m => m Bool
configure = bracketIO I.configure

create :: MonadFlipper m => String -> m ()
create = bracketIO . I.create

delete :: MonadFlipper m => String -> m ()
delete = bracketIO . I.delete

size :: MonadFlipper m => String -> m Word32
size = bracketIO . I.size

open :: MonadFlipper m => String -> Word32 -> m ()
open = (bracketIO .) . I.open

push :: (Bufferable b, MonadFlipper m) => b -> m ()
push = bracketIO . I.push . runPut . put

pull :: (Bufferable b, MonadFlipper m) => m (Either String b)
pull = runGetWith get (bracketIO . I.pull) emptyBuffer

close :: MonadFlipper m => m ()
close = bracketIO I.close

format :: MonadFlipper m => m ()
format = bracketIO I.format
