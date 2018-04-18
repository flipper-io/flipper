{-|
Module      : Flipper.Internal.SPI
Description : Internal SPI Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.SPI (
    configure
  , enable
  , disable
  , put
  , get
  , push
  , pull
  ) where

import Data.Word

import Flipper.Internal.Buffer
import Flipper.Internal.Utils

import Foreign.ForeignPtr
import Foreign.Ptr

configure :: IO Bool
configure = retSuc <$> c_spi_configure

enable :: IO ()
enable = c_spi_enable

disable :: IO ()
disable = c_spi_disable

put :: Word8 -> IO ()
put = c_spi_put

get :: IO Word8
get = c_spi_get

push :: Buffer -> IO ()
push (Buffer p o l) = withForeignPtr p $ \p' ->
    c_spi_write (plusPtr p' o) (fromIntegral l)

pull :: Int -> IO Buffer
pull l
    | l <= 0    = error "pull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_spi_read p' (fromIntegral l))
                     return b

foreign import ccall safe "flipper/spi/spi.h spi_configure"
    c_spi_configure :: IO Word32

foreign import ccall safe "flipper/spi.h spi_enable"
    c_spi_enable :: IO ()

foreign import ccall safe "flipper/spi.h spi_disable"
    c_spi_disable :: IO ()

foreign import ccall safe "flipper/spi.h spi_put"
    c_spi_put :: Word8 -> IO ()

foreign import ccall safe "flipper/spi.h spi_get"
    c_spi_get :: IO Word8

foreign import ccall safe "flipper/spi.h spi_write"
    c_spi_write :: Ptr Word8 -> Word32 -> IO ()

foreign import ccall safe "flipper/spi.h spi_read"
    c_spi_read :: Ptr Word8 -> Word32 -> IO ()
