{-|
Module      : Flipper.Internal.AT45
Description : Internal AT45 Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.AT45 (
    enable
  , disable
  , reset
  , alloc
  , free
  , format
  , push
  , pull
  , pullAdvance
  ) where

import Flipper.Internal.Buffer
import Flipper.Internal.FS

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal.Alloc hiding (free)

import Data.IORef

import Data.Word

enable :: IO ()
enable = c_at45_enable

disable :: IO ()
disable = c_at45_disable

reset :: IO ()
reset = c_at45_reset

read :: FSHandle -> IO ()
read = c_at45_read . unFSHandle

get :: IO Word8
get = c_at45_get

alloc :: Word32 -> IO FSHandle
alloc = (FSHandle <$>) . c_at45_alloc

free :: FSHandle -> IO ()
free = c_at45_free . unFSHandle

format :: IO ()
format = c_at45_format

push :: Buffer -> FSHandle -> IO ()
push (Buffer p o l) (FSHandle h) = withForeignPtr p $ \p' ->
    c_at45_push (plusPtr p' o) (fromIntegral l) h

pull :: FSHandle -> Int -> IO Buffer
pull (FSHandle h) l
    | l <= 0    = error "pull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_at45_pull p' (fromIntegral l) h)
                     return b

pullAdvance :: FSHandle -> IO (Int -> IO Buffer)
pullAdvance (FSHandle h) = pullOnce <$> newIORef h
    where pullOnce hp l
                | l <= 0    = error "pullAdvance: length must be greater than zero."
                | otherwise = do h' <- readIORef hp
                                 b@(Buffer p _ _) <- allocBufferSafe l
                                 withForeignPtr p (\p' -> c_at45_pull p' (fromIntegral l) h')
                                 modifyIORef' hp (+ (fromIntegral l))
                                 return b

-- | Don't use this, it allocates memory outside of the Haskell heap. Use 'pull'
--   instead.
dereference :: FSHandle -> Int -> IO Buffer
dereference (FSHandle h) l
    | l <= 0 = error "dereference: length must be greater than zero."
    | otherwise = do p  <- c_at45_dereference h (fromIntegral l)
                     fp <- newForeignPtr finalizerFree p
                     return $ Buffer fp 0 l

foreign import ccall safe "flipper/at45.h at45_enable"
    c_at45_enable :: IO ()

foreign import ccall safe "flipper/at45.h at45_disable"
    c_at45_disable :: IO ()

foreign import ccall safe "flipper/at45.h at45_reset"
    c_at45_reset :: IO ()

foreign import ccall safe "flipper/at45.h at45_read"
    c_at45_read :: Word32 -> IO ()

foreign import ccall safe "flipper/at45.c at45_get"
    c_at45_get :: IO Word8

foreign import ccall safe "flipper/at45.h at45_alloc"
    c_at45_alloc :: Word32 -> IO Word32

foreign import ccall safe "flipper/at45.h at45_free"
    c_at45_free :: Word32 -> IO ()

foreign import ccall safe "flipper/at45.h at45_format"
    c_at45_format :: IO ()

foreign import ccall safe "flipper/at45.h at45_push"
    c_at45_push :: Ptr Word8 -> Word32 -> Word32 -> IO ()

foreign import ccall safe "flipper/at45.h at45_pull"
    c_at45_pull :: Ptr Word8 -> Word32 -> Word32 -> IO ()

foreign import ccall safe "flipper/at45.h at45_dereference"
    c_at45_dereference :: Word32 -> CSize -> IO (Ptr Word8)
