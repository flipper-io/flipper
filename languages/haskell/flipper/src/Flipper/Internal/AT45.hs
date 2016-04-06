module Flipper.Internal.AT45 where

import Flipper.Buffer
import Flipper.Internal.FS

import Foreign.ForeignPtr
import Foreign.Ptr

import Data.Word

enable :: IO ()
enable = c_at45_enable

disable :: IO ()
disable = c_at45_disable

reset :: IO ()
reset = c_at45_reset

alloc :: Word32 -> IO FSHandle
alloc = (FSHandle <$>) . c_at45_alloc

free :: FSHandle -> IO ()
free = c_at45_free . unFSHandle

format :: IO ()
format = c_at45_format

push :: Buffer -> FSHandle -> IO ()
push (Buffer p o l) (FSHandle h) = withForeignPtr p
    (\p' -> c_at45_push (plusPtr p' o) (fromIntegral l) h)

pull :: FSHandle -> Int -> IO Buffer
pull (FSHandle h) l
    | l <= 0    = error "pull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBuffer l
                     withForeignPtr p (\p' -> c_at45_pull (castPtr p') (fromIntegral l) h)
                     return b

foreign import ccall safe "flipper/at45.h at45_enable"
    c_at45_enable :: IO ()

foreign import ccall safe "flipper/at45.h at45_disable"
    c_at45_disable :: IO ()

foreign import ccall safe "flipper/at45.h at45_reset"
    c_at45_reset :: IO ()

--foreign import ccall safe "flipper/at45.h at45_read"
--    c_at45_read :: Word32 -> IO ()

foreign import ccall safe "flipper/at45.h at45_alloc"
    c_at45_alloc :: Word32 -> IO Word32

foreign import ccall safe "flipper/at45.h at45_free"
    c_at45_free :: Word32 -> IO ()

foreign import ccall safe "flipper/at45.h at45_format"
    c_at45_format :: IO ()

foreign import ccall safe "flipper/at45.h at45_push"
    c_at45_push :: Ptr () -> Word32 -> Word32 -> IO ()

foreign import ccall safe "flipper/at45.h at45_pull"
    c_at45_pull :: Ptr () -> Word32 -> Word32 -> IO ()
