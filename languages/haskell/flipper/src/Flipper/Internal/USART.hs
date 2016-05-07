module Flipper.Internal.USART where

import Data.Word

import Flipper.Buffer

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr

usart0Enable :: IO ()
usart0Enable = c_usart0_enable

usart0Disable :: IO ()
usart0Disable = c_usart0_disable

usart0Ready :: IO Bool
usart0Ready = toBool <$> c_usart0_ready

usart0Put :: Word8 -> IO ()
usart0Put = c_usart0_put

usart0Get :: IO Word8
usart0Get = c_usart0_get

usart0Push :: Buffer -> IO ()
usart0Push (Buffer p o l) = withForeignPtr p $ \p' ->
    c_usart0_push (castPtr (plusPtr p' o)) (fromIntegral l)

usart0Pull :: Int -> IO Buffer
usart0Pull l
    | l <= 0    = error "usart0Pull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_usart0_pull (castPtr p') (fromIntegral l))
                     return b

usart1Enable :: IO ()
usart1Enable = c_usart1_enable

usart1Disable :: IO ()
usart1Disable = c_usart1_disable

usart1Ready :: IO Bool
usart1Ready = toBool <$> c_usart1_ready

usart1Put :: Word8 -> IO ()
usart1Put = c_usart1_put

usart1Get :: IO Word8
usart1Get = c_usart1_get

usart1Push :: Buffer -> IO ()
usart1Push (Buffer p o l) = withForeignPtr p $ \p' ->
    c_usart1_push (castPtr (plusPtr p' o)) (fromIntegral l)

usart1Pull :: Int -> IO Buffer
usart1Pull l
    | l <= 0    = error "usart1Pull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_usart1_pull (castPtr p') (fromIntegral l))
                     return b

dbguEnable :: IO ()
dbguEnable = c_dbgu_enable

dbguDisable :: IO ()
dbguDisable = c_dbgu_disable

dbguReady :: IO Bool
dbguReady = toBool <$> c_dbgu_ready

dbguPut :: Word8 -> IO ()
dbguPut = c_dbgu_put

dbguGet :: IO Word8
dbguGet = c_dbgu_get

dbguPush :: Buffer -> IO ()
dbguPush (Buffer p o l) = withForeignPtr p $ \p' ->
    c_dbgu_push (castPtr (plusPtr p' o)) (fromIntegral l)

dbguPull :: Int -> IO Buffer
dbguPull l
    | l <= 0    = error "dbguPull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_dbgu_pull (castPtr p') (fromIntegral l))
                     return b

foreign import ccall safe "flipper/usart.h usart0_enable"
    c_usart0_enable :: IO ()

foreign import ccall safe "flipper/usart.h usart0_disable"
    c_usart0_disable :: IO ()

foreign import ccall safe "flipper/usart.h usart0_ready"
    c_usart0_ready :: IO Word8

foreign import ccall safe "flipper/usart.h usart0_put"
    c_usart0_put :: Word8 -> IO ()

foreign import ccall safe "flipper/usart.h usart0_get"
    c_usart0_get :: IO Word8

foreign import ccall safe "flipper/usart.h usart0_push"
    c_usart0_push :: Ptr () -> CSize -> IO ()

foreign import ccall safe "flipper/usart.h usart0_pull"
    c_usart0_pull :: Ptr () -> CSize -> IO ()

foreign import ccall safe "flipper/usart.h usart1_enable"
    c_usart1_enable :: IO ()

foreign import ccall safe "flipper/usart.h usart1_disable"
    c_usart1_disable :: IO ()

foreign import ccall safe "flipper/usart.h usart1_ready"
    c_usart1_ready :: IO Word8

foreign import ccall safe "flipper/usart.h usart1_put"
    c_usart1_put :: Word8 -> IO ()

foreign import ccall safe "flipper/usart.h usart1_get"
    c_usart1_get :: IO Word8

foreign import ccall safe "flipper/usart.h usart1_push"
    c_usart1_push :: Ptr () -> CSize -> IO ()

foreign import ccall safe "flipper/usart.h usart1_pull"
    c_usart1_pull :: Ptr () -> CSize -> IO ()

foreign import ccall safe "flipper/usart.h dbgu_enable"
    c_dbgu_enable :: IO ()

foreign import ccall safe "flipper/usart.h dbgu_disable"
    c_dbgu_disable :: IO ()

foreign import ccall safe "flipper/usart.h dbgu_ready"
    c_dbgu_ready :: IO Word8

foreign import ccall safe "flipper/usart.h dbgu_put"
    c_dbgu_put :: Word8 -> IO ()

foreign import ccall safe "flipper/usart.h dbgu_get"
    c_dbgu_get :: IO Word8

foreign import ccall safe "flipper/usart.h dbgu_push"
    c_dbgu_push :: Ptr () -> CSize -> IO ()

foreign import ccall safe "flipper/usart.h dbgu_pull"
    c_dbgu_pull :: Ptr () -> CSize -> IO ()

