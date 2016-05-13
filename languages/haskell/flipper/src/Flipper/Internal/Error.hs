{-|
Module      : Flipper.Internal.Error
Description : Internal Error Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.Error (
    FlipperError(..)
  , withold
  , disclose
  , raise
  , get
  , clear
  ) where


import Data.Word

import Flipper.Internal.Utils

import Foreign.C.String
import Foreign.Ptr

data FlipperError = OK
                  | FMRChecksumWrong
                  | MemAllocFailed
                  | TooManyArgs
                  | FVMLoadFailed
                  | FVMSymbolFailed
                  | SocketOpenFailed
                  | SocketConnectFailed
                  | FlipperUnbound
                  | FlipperNotFound
                  | HIDManagerFailed
                  | HIDManagerNoDevice
                  | HIDTooManyDevices
                  | HIDOpenDeviceFailed
                  | HIDDeviceDisconnected
                  | HIDWriteFailed
                  | HIDTimeout
                  | IOKitDictionaryError
                  | DynLibNotFound
                  | DynLibLoadFailure
                  | DynLibAlreadyLoaded
                  | FileOpenFailure
                  | AddFileFailure
                  | NoFileFailure
                  | Unimplemented
                  | Unknown
                  deriving (Eq, Ord, Show)

errorCode :: FlipperError -> Word16
errorCode OK                    = 0
errorCode FMRChecksumWrong      = 1
errorCode MemAllocFailed        = 2
errorCode TooManyArgs           = 3
errorCode FVMLoadFailed         = 4
errorCode FVMSymbolFailed       = 5
errorCode SocketOpenFailed      = 6
errorCode SocketConnectFailed   = 7
errorCode FlipperUnbound        = 8
errorCode FlipperNotFound       = 9
errorCode HIDManagerFailed      = 10
errorCode HIDManagerNoDevice    = 11
errorCode HIDTooManyDevices     = 12
errorCode HIDOpenDeviceFailed   = 13
errorCode HIDDeviceDisconnected = 14
errorCode HIDWriteFailed        = 15
errorCode HIDTimeout            = 16
errorCode IOKitDictionaryError  = 17
errorCode DynLibNotFound        = 18
errorCode DynLibLoadFailure     = 19
errorCode DynLibAlreadyLoaded   = 20
errorCode FileOpenFailure       = 21
errorCode AddFileFailure        = 22
errorCode NoFileFailure         = 23
errorCode Unimplemented         = 24
errorCode Unknown               = maxBound

codeError :: Word16 -> FlipperError
codeError 0  = OK
codeError 1  = FMRChecksumWrong
codeError 2  = MemAllocFailed
codeError 3  = TooManyArgs
codeError 4  = FVMLoadFailed
codeError 5  = FVMSymbolFailed
codeError 6  = SocketOpenFailed
codeError 7  = SocketConnectFailed
codeError 8  = FlipperUnbound
codeError 9  = FlipperNotFound
codeError 10 = HIDManagerFailed
codeError 11 = HIDManagerNoDevice
codeError 12 = HIDTooManyDevices
codeError 13 = HIDOpenDeviceFailed
codeError 14 = HIDDeviceDisconnected
codeError 15 = HIDWriteFailed
codeError 16 = HIDTimeout
codeError 17 = IOKitDictionaryError
codeError 18 = DynLibNotFound
codeError 19 = DynLibLoadFailure
codeError 20 = DynLibAlreadyLoaded
codeError 21 = FileOpenFailure
codeError 22 = AddFileFailure
codeError 23 = NoFileFailure
codeError 24 = Unimplemented
codeError _  = Unknown

withold :: IO ()
withold = c_error_withold

disclose :: IO ()
disclose = c_error_disclose

raise :: FlipperError -> IO ()
raise e = c_error_raise (errorCode e) nullPtr

get :: IO FlipperError
get = codeError <$> c_error_get

clear :: IO ()
clear = c_error_clear

foreign import ccall safe "flipper/error/error.h error_withold"
    c_error_withold :: IO ()

foreign import ccall safe "flipper/error/error.h error_disclose"
    c_error_disclose :: IO ()

foreign import ccall safe "flipper/error/error.h error_raise"
    c_error_raise :: Word16 -> CString -> IO ()

foreign import ccall safe "flipper/error/error.h error_get"
    c_error_get :: IO Word16

foreign import ccall safe "flipper/error/error.h error_clear"
    c_error_clear :: IO ()
