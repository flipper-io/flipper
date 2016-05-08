module Flipper.Internal.SPI where

import Data.Word

import Flipper.Buffer

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr

enable :: IO ()
enable = c_spi_enable

disable :: IO ()
disable = c_spi_disable

ready :: IO Bool
ready = toBool <$> c_spi_ready

put :: Word8 -> IO ()
put = c_spi_put

get :: IO Word8
get = c_spi_get

push :: Buffer -> IO ()
push (Buffer p o l) = withForeignPtr p $ \p' ->
    c_spi_push (plusPtr p' o) (fromIntegral l)

pull :: Int -> IO Buffer
pull l
    | l <= 0    = error "pull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_spi_pull p' (fromIntegral l))
                     return b

foreign import ccall safe "flipper/spi.h spi_enable"
    c_spi_enable :: IO ()

foreign import ccall safe "flipper/spi.h spi_disable"
    c_spi_disable :: IO ()

foreign import ccall safe "flipper/spi.h spi_ready"
    c_spi_ready :: IO Word8

foreign import ccall safe "flipper/spi.h spi_put"
    c_spi_put :: Word8 -> IO ()

foreign import ccall safe "flipper/spi.h spi_get"
    c_spi_get :: IO Word8

foreign import ccall safe "flipper/spi.h spi_push"
    c_spi_push :: Ptr Word8 -> CSize -> IO ()

foreign import ccall safe "flipper/spi.h spi_pull"
    c_spi_pull :: Ptr Word8 -> CSize -> IO ()
