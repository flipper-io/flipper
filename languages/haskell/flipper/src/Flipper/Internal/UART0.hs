{-|
Module      : Flipper.Internal.UART0
Description : Internal UART0 Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.UART0 (
    configure
  , enable
  , disable
  , ready
  , put
  , get
  , push
  , pull
  ) where

import Data.Word

import Flipper.Internal.Buffer
import Flipper.Internal.Utils

import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr

configure :: IO Bool
configure = retSuc <$> c_uart0_configure

enable :: IO ()
enable = c_uart0_enable

disable :: IO ()
disable = c_uart0_disable

ready :: IO Bool
ready = toBool <$> c_uart0_ready

put :: Word8 -> IO ()
put = c_uart0_put

get :: IO Word8
get = c_uart0_get

push :: Buffer -> IO ()
push (Buffer p o l) = withForeignPtr p $ \p' ->
   c_uart0_push (plusPtr p' o) (fromIntegral l)

pull :: Int -> IO Buffer
pull l
    | l <= 0    = error "pull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_uart0_pull p' (fromIntegral l))
                     return b

foreign import ccall safe "flipper/uart0/uart0.h uart0_configure"
    c_uart0_configure :: IO Word32

foreign import ccall safe "flipper/carbon/modules/uart0.h uart0_enable"
    c_uart0_enable :: IO ()

foreign import ccall safe "flipper/carbon/modules/uart0.h uart0_disable"
    c_uart0_disable :: IO ()

foreign import ccall safe "flipper/carbon/modules/uart0.h uart0_ready"
    c_uart0_ready :: IO Word8

foreign import ccall safe "flipper/carbon/modules/uart0.h uart0_put"
    c_uart0_put :: Word8 -> IO ()

foreign import ccall safe "flipper/carbon/modules/uart0.h uart0_get"
    c_uart0_get :: IO Word8

foreign import ccall safe "flipper/carbon/modules/uart0.h uart0_push"
    c_uart0_push :: Ptr Word8 -> Word32 -> IO ()

foreign import ccall safe "flipper/carbon/modules/uart0.h uart0_pull"
    c_uart0_pull :: Ptr Word8 -> Word32 -> IO ()
