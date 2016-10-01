{-|
Module      : Flipper.Internal.UART
Description : Internal UART Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.UART (
    uartEnable
  , uartDisable
  , uartPut
  , uartGet
  , uartPush
  , uartPull
  ) where

import Data.Word

import Flipper.Internal.Buffer

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr

uartEnable :: IO ()
uartEnable = c_uart_enable

uartDisable :: IO ()
uartDisable = c_uart_disable

uartReady :: IO Bool
uartReady = toBool <$> c_uart_ready

uartPut :: Word8 -> IO ()
uartPut = c_uart_put

uartGet :: IO Word8
uartGet = c_uart_get

uartPush :: Buffer -> IO ()
uartPush (Buffer p o l) = withForeignPtr p $ \p' ->
   c_uart_push (plusPtr p' o) (fromIntegral l)

uartPull :: Int -> IO Buffer
uartPull l
    | l <= 0    = error "uartPull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_uart_pull p' (fromIntegral l))
                     return b

foreign import ccall safe "flipper/uart.h uart_enable"
    c_uart_enable :: IO ()

foreign import ccall safe "flipper/uart.h uart_disable"
    c_uart_disable :: IO ()

foreign import ccall safe "flipper/uart.h uart_ready"
    c_uart_ready :: IO Word8

foreign import ccall safe "flipper/uart.h uart_put"
    c_uart_put :: Word8 -> IO ()

foreign import ccall safe "flipper/uart.h uart_get"
    c_uart_get :: IO Word8

foreign import ccall safe "flipper/uart.h uart_push"
    c_uart_push :: Ptr Word8 -> Word32 -> IO ()

foreign import ccall safe "flipper/uart.h uart_pull"
    c_uart_pull :: Ptr Word8 -> Word32 -> IO ()
