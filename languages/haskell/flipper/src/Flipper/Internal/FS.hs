{-|
Module      : Flipper.Internal.FS
Description : Internal FS Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.FS (
    FSHandle(..)
  , checksum
  ) where

import Data.Word

import Flipper.Internal.Buffer

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

import System.IO.Unsafe

newtype FSHandle = FSHandle { unFSHandle :: Word32 }

-- Not totally implemented yet.

checksum :: Buffer -> Word16
checksum (Buffer fp o l) = unsafeDupablePerformIO $
    withForeignPtr fp (\p -> return (c_fs_checksum (plusPtr p o) (fromIntegral l)))

foreign import ccall safe "flipper/fs/crc.h checksum"
    c_fs_checksum :: Ptr Word8 -> CSize -> Word16
