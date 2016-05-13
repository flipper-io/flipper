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
  , checksum
  ) where

import Data.Word

import Flipper.Bufferable
import Flipper.Put

import qualified Flipper.Internal.FS as I

checksum :: Bufferable b => b -> Word16
checksum = I.checksum . runPut . put
