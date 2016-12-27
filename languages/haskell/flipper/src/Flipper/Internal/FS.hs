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

import Flipper.Internal.Buffer
import Flipper.Internal.Utils

import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr

configure :: IO Bool
configure = retSuc <$> c_fs_configure

create :: String -> IO ()
create n = withCString n c_fs_create

delete :: String -> IO ()
delete n = withCString n c_fs_delete

size :: String -> IO Word32
size n = withCString n c_fs_size

open :: String -> Word32 -> IO ()
open n s = withCString n $ \n' -> c_fs_open n' s

push :: Buffer -> IO ()
push (Buffer p o l) = withForeignPtr p $ \p' -> c_fs_push (p' `plusPtr` o)
                                                          (fromIntegral l)

pull :: Int -> IO Buffer
pull l
    | l <= 0    = error "FS.pull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_fs_pull p' (fromIntegral l))
                     return b

close :: IO ()
close = c_fs_close

format :: IO ()
format = c_fs_format

foreign import ccall safe "flipper/fs/fs.h fs_configure"
    c_fs_configure :: IO Word32

foreign import ccall safe "flipper/fs.h fs_create"
    c_fs_create :: Ptr CChar -> IO ()

foreign import ccall safe "flipper/fs.h fs_delete"
    c_fs_delete :: Ptr CChar -> IO ()

foreign import ccall safe "flipper/fs.h fs_size"
    c_fs_size :: Ptr CChar -> IO Word32

foreign import ccall safe "flipper/fs.h fs_open"
    c_fs_open :: Ptr CChar -> Word32 -> IO ()

foreign import ccall safe "flipper/fs.h fs_push"
    c_fs_push :: Ptr Word8 -> Word32 -> IO ()

foreign import ccall safe "flipper/fs.h fs_pull"
    c_fs_pull :: Ptr Word8 -> Word32 -> IO ()

foreign import ccall safe "flipper/fs.h fs_close"
    c_fs_close :: IO ()

foreign import ccall safe "flipper/fs.h fs_format"
    c_fs_format :: IO ()
