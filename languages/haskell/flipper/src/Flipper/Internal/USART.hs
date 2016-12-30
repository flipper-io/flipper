{-|
Module      : Flipper.Internal.USART
Description : Internal USART Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.USART (
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
configure = retSuc <$> c_usart_configure

enable :: IO ()
enable = c_usart_enable

disable :: IO ()
disable = c_usart_disable

ready :: IO Bool
ready = toBool <$> c_usart_ready

put :: Word8 -> IO ()
put = c_usart_put

get :: IO Word8
get = c_usart_get

push :: Buffer -> IO ()
push (Buffer p o l) = withForeignPtr p $ \p' ->
   c_usart_push (plusPtr p' o) (fromIntegral l)

pull :: Int -> IO Buffer
pull l
    | l <= 0    = error "pull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_usart_pull p' (fromIntegral l))
                     return b

foreign import ccall safe "flipper/usart/usart.h usart_configure"
    c_usart_configure :: IO Word32

foreign import ccall safe "flipper/carbon/modules/usart.h usart_enable"
    c_usart_enable :: IO ()

foreign import ccall safe "flipper/carbon/modules/usart.h usart_disable"
    c_usart_disable :: IO ()

foreign import ccall safe "flipper/carbon/modules/usart.h usart_ready"
    c_usart_ready :: IO Word8

foreign import ccall safe "flipper/carbon/modules/usart.h usart_put"
    c_usart_put :: Word8 -> IO ()

foreign import ccall safe "flipper/carbon/modules/usart.h usart_get"
    c_usart_get :: IO Word8

foreign import ccall safe "flipper/carbon/modules/usart.h usart_push"
    c_usart_push :: Ptr Word8 -> Word32 -> IO ()

foreign import ccall safe "flipper/carbon/modules/usart.h usart_pull"
    c_usart_pull :: Ptr Word8 -> Word32 -> IO ()
