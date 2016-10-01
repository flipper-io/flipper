{-|
Module      : Flipper.Internal.FS
Description : Internal FS Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flipper.Internal.FS (
    FSHandle(..)
  , format
  , create
  , remove
  , rename
  , withGet
  , withPut
  , getHandle
  ) where

import Data.Word

import Flipper.Internal.Buffer

import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr

import System.IO.Unsafe

-- | Flipper file system reference.
newtype FSHandle = FSHandle { unFSHandle :: Word32 }
                 deriving (Eq, Ord, Show)

format :: IO ()
format = c_fs_format

create :: String -> Buffer -> IO ()
create s (Buffer p o l) = withForeignPtr p $ \p' -> withCString s $ \s' ->
    c_fs_create s' (p' `plusPtr` o) (fromIntegral l)

remove :: String -> IO ()
remove s = withCString s c_fs_remove

rename :: String -> String -> IO ()
rename t f = withCString t $ \t' -> withCString f $ \f' -> c_fs_rename t' f'

withGet :: String -> (IO Word8 -> IO a) -> IO a
withGet s f = withCString s $ \s' -> do
    c_fs_read s'
    r <- f c_fs_get
    error "implement detach!"
    return r

withPut :: String -> ((Word8 -> IO ()) -> IO a) -> IO a
withPut s f = withCString s $ \s' -> do
    c_fs_write s'
    r <- f c_fs_put
    error "implement detach!"
    return r

getHandle :: String -> IO FSHandle
getHandle s = FSHandle <$> withCString s c_fs_data

foreign import ccall safe "flipper/fs.h fs_format"
    c_fs_format :: IO ()

foreign import ccall safe "flipper/fs.h fs_create"
    c_fs_create :: CString -> Ptr Word8 -> CSize -> IO ()

foreign import ccall safe "flipper/fs.h fs_remove"
    c_fs_remove :: CString -> IO ()

foreign import ccall safe "flipper/fs.h fs_rename"
    c_fs_rename :: CString -> CString -> IO ()

foreign import ccall safe "flipper/fs.h fs_write"
    c_fs_write :: CString -> IO ()

foreign import ccall safe "flipper/fs.h fs_read"
    c_fs_read :: CString -> IO ()

foreign import ccall safe "flipper/fs.h fs_get"
    c_fs_get :: IO Word8

foreign import ccall safe "flipper/fs.h fs_put"
    c_fs_put :: Word8 -> IO ()

foreign import ccall safe "flipper/fs.h fs_data"
    c_fs_data :: CString -> IO Word32
