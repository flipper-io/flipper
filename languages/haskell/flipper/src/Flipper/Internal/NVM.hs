{-|
Module      : Flipper.Internal.NVM
Description : Internal NVM Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.NVM (
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
enable = c_nvm_enable

disable :: IO ()
disable = c_nvm_disable

reset :: IO ()
reset = c_nvm_reset

read :: FSHandle -> IO ()
read = c_nvm_read . unFSHandle

get :: IO Word8
get = c_nvm_get

alloc :: Word32 -> IO FSHandle
alloc = (FSHandle <$>) . c_nvm_alloc

free :: FSHandle -> IO ()
free = c_nvm_free . unFSHandle

format :: IO ()
format = c_nvm_format

push :: Buffer -> FSHandle -> IO ()
push (Buffer p o l) (FSHandle h) = withForeignPtr p $ \p' ->
    c_nvm_push (plusPtr p' o) (fromIntegral l) h

pull :: FSHandle -> Int -> IO Buffer
pull (FSHandle h) l
    | l <= 0    = error "pull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_nvm_pull p' (fromIntegral l) h)
                     return b

pullAdvance :: FSHandle -> IO (Int -> IO Buffer)
pullAdvance (FSHandle h) = pullOnce <$> newIORef h
    where pullOnce hp l
                | l <= 0    = error "pullAdvance: length must be greater than zero."
                | otherwise = do h' <- readIORef hp
                                 b@(Buffer p _ _) <- allocBufferSafe l
                                 withForeignPtr p (\p' -> c_nvm_pull p' (fromIntegral l) h')
                                 modifyIORef' hp (+ (fromIntegral l))
                                 return b

-- | Don't use this, it allocates memory outside of the Haskell heap. Use 'pull'
--   instead.
dereference :: FSHandle -> Int -> IO Buffer
dereference (FSHandle h) l
    | l <= 0 = error "dereference: length must be greater than zero."
    | otherwise = do p  <- c_nvm_dereference h (fromIntegral l)
                     fp <- newForeignPtr finalizerFree p
                     return $ Buffer fp 0 l

foreign import ccall safe "flipper/nvm.h nvm_enable"
    c_nvm_enable :: IO ()

foreign import ccall safe "flipper/nvm.h nvm_disable"
    c_nvm_disable :: IO ()

foreign import ccall safe "flipper/nvm.h nvm_reset"
    c_nvm_reset :: IO ()

foreign import ccall safe "flipper/nvm.h nvm_read"
    c_nvm_read :: Word32 -> IO ()

foreign import ccall safe "flipper/nvm.c nvm_get"
    c_nvm_get :: IO Word8

foreign import ccall safe "flipper/nvm.h nvm_alloc"
    c_nvm_alloc :: Word32 -> IO Word32

foreign import ccall safe "flipper/nvm.h nvm_free"
    c_nvm_free :: Word32 -> IO ()

foreign import ccall safe "flipper/nvm.h nvm_format"
    c_nvm_format :: IO ()

foreign import ccall safe "flipper/nvm.h nvm_push"
    c_nvm_push :: Ptr Word8 -> Word32 -> Word32 -> IO ()

foreign import ccall safe "flipper/nvm.h nvm_pull"
    c_nvm_pull :: Ptr Word8 -> Word32 -> Word32 -> IO ()

foreign import ccall safe "flipper/nvm.h nvm_dereference"
    c_nvm_dereference :: Word32 -> CSize -> IO (Ptr Word8)
