module Flipper.Internal.FS where

import Data.Word

import Flipper.Buffer

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

import System.IO.Unsafe

newtype FSHandle = FSHandle { unFSHandle :: Word32 }

-- Not totally implemented yet.

checksum :: Buffer -> Word16
checksum (Buffer fp o l) = unsafeDupablePerformIO $
    withForeignPtr fp (\p -> return (c_fs_checksum (plusPtr p (fromIntegral o)) (fromIntegral l)))

foreign import ccall safe "flipper/fs/crc.h checksum"
    c_fs_checksum :: Ptr Word8 -> CSize -> Word16
