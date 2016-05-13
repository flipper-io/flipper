{-|
Module      : Flipper.AT45
Description : AT45 Flash Memory Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.AT45 (
    enable
  , disable
  , reset
  , alloc
  , free
  , format
  , push
  , pull
  ) where

import Data.Word

import Flipper.Buffer
import Flipper.Bufferable
import Flipper.FS
import Flipper.Get
import Flipper.Put
import Flipper.MonadFlipper

import qualified Flipper.Internal.AT45 as I

enable :: MonadFlipper m => m ()
enable = bracketIO I.enable

disable :: MonadFlipper m => m ()
disable = bracketIO I.disable

reset :: MonadFlipper m => m ()
reset = bracketIO I.reset

alloc :: MonadFlipper m =>  Word32 -> m FSHandle
alloc = bracketIO . I.alloc

free :: MonadFlipper m => FSHandle -> m ()
free = bracketIO . I.free

format :: MonadFlipper m => m ()
format = bracketIO I.format

push :: (Bufferable b, MonadFlipper m) => b -> FSHandle -> m ()
push b = bracketIO  . I.push (runPut (put b))

pull :: (Bufferable b, MonadFlipper m) => FSHandle -> m (Either String b)
pull h = (bracketIO (I.pullAdvance h)) >>= (\p -> runGetWith get (bracketIO . p) emptyBuffer)
