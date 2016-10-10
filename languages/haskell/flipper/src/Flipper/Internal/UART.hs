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
    usartEnable
  , usartDisable
  , usartPut
  , usartGet
  , usartPush
  , usartPull
  ) where

import Data.Word

import Flipper.Internal.Buffer

import Foreign.ForeignPtr
import Foreign.Ptr

usartEnable :: IO ()
usartEnable = c_usart_enable

usartDisable :: IO ()
usartDisable = c_usart_disable

usartPut :: Word8 -> IO ()
usartPut = c_usart_put

usartGet :: IO Word8
usartGet = c_usart_get

usartPush :: Buffer -> IO ()
usartPush (Buffer p o l) = withForeignPtr p $ \p' ->
   c_usart_push (plusPtr p' o) (fromIntegral l)

usartPull :: Int -> IO Buffer
usartPull l
    | l <= 0    = error "usartPull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_usart_pull p' (fromIntegral l))
                     return b

foreign import ccall safe "flipper/usart.h usart_enable"
    c_usart_enable :: IO ()

foreign import ccall safe "flipper/usart.h usart_disable"
    c_usart_disable :: IO ()

foreign import ccall safe "flipper/usart.h usart_put"
    c_usart_put :: Word8 -> IO ()

foreign import ccall safe "flipper/usart.h usart_get"
    c_usart_get :: IO Word8

foreign import ccall safe "flipper/usart.h usart_push"
    c_usart_push :: Ptr Word8 -> Word32 -> IO ()

foreign import ccall safe "flipper/usart.h usart_pull"
    c_usart_pull :: Ptr Word8 -> Word32 -> IO ()
