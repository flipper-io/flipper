module Flipper.Internal.USB where

import Data.Word

import Flipper.Buffer

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr

usbEnable :: IO ()
usbEnable = c_usb_enable

usbDisable :: IO ()
usbDisable = c_usb_disable

usbReady :: IO Bool
usbReady = toBool <$> c_usb_ready

usbPut :: Word8 -> IO ()
usbPut = c_usb_put

usbGet :: IO Word8
usbGet = c_usb_get

usbPush :: Buffer -> IO ()
usbPush (Buffer p o l) = withForeignPtr p $ \p' ->
    c_usb_push (castPtr (plusPtr p' o)) (fromIntegral l)

usbPull :: Int -> IO Buffer
usbPull l
    | l <= 0    = error "usbPull: length must be greater than zero."
    | otherwise = do b@(Buffer p _ _) <- allocBufferSafe l
                     withForeignPtr p (\p' -> c_usb_pull (castPtr p') (fromIntegral l))
                     return b

foreign import ccall safe "flipper/usb.h usb_enable"
    c_usb_enable :: IO ()

foreign import ccall safe "flipper/usb.h usb_disable"
    c_usb_disable :: IO ()

foreign import ccall safe "flipper/usb.h usb_ready"
    c_usb_ready :: IO Word8

foreign import ccall safe "flipper/usb.h usb_put"
    c_usb_put :: Word8 -> IO ()

foreign import ccall safe "flipper/usb.h usb_get"
    c_usb_get :: IO Word8

foreign import ccall safe "flipper/usb.h usb_push"
    c_usb_push :: Ptr () -> CSize -> IO ()

foreign import ccall safe "flipper/usb.h usb_pull"
    c_usb_pull :: Ptr () -> CSize -> IO ()
