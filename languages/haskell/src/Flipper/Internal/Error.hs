{-|
Module      : Flipper.Internal.Error
Description : Internal Error Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

{-# LANGUAGE DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           #-}

module Flipper.Internal.Error (
    FlipperError(..)
  , configure
  , pause
  , resume
  , raise
  , get
  , clear
  ) where

import Control.DeepSeq

import Data.Data

import Data.Word

import Foreign.C.String
import Foreign.Ptr

import Flipper.Internal.Utils

import GHC.Generics

-- | An error condition, reported by the device or occuring within the
--   @libflipper@ library.
data FlipperError = OK                -- ^ All clear.
                  | MemAllocFailed    -- ^ Memory allocation failed.
                  | Null              -- ^ Null pointer error.
                  | Overflow          -- ^ Overflow.
                  | NoDevice          -- ^ invalid device available.
                  | NotAttached       -- ^ Device not attached.
                  | AlreadyAttached   -- ^ Device already attached.
                  | FileAlreadyExists -- ^ File already exists.
                  | FileNotFound      -- ^ File not found.
                  | FMRPacketOverflow -- ^ FMR packet buffer overflow.
                  | FMRError          -- ^ Unspecified FMR error.
                  | Endpoint          -- ^ Unspecified communication endpoint
                                      --   error.
                  | USB               -- ^ @libusb@ error.
                  | Communication     -- ^ Device communication error.
                  | Socket            -- ^ Socket error.
                  | Module            -- ^ Module not found.
                  | Resolution        -- ^ Module dispatch resolution error.
                  | NoString          -- ^ String not found.
                  | Checksum          -- ^ Checksum error.
                  | Name              -- ^ Name not found.
                  | Configuration     -- ^ Configuration read error.
                  | Acknowledge       -- ^ Device failed to acknowledge.
                  | Unknown           -- ^ Unknown error.
                  deriving ( Eq
                           , Ord
                           , Read
                           , Show
                           , Enum
                           , Generic
                           , NFData
                           , Data
                           , Typeable
                           )

errorCode :: FlipperError -> Word16
errorCode OK                = 0
errorCode MemAllocFailed    = 1
errorCode Null              = 2
errorCode Overflow          = 3
errorCode NoDevice          = 4
errorCode NotAttached       = 5
errorCode AlreadyAttached   = 6
errorCode FileAlreadyExists = 7
errorCode FileNotFound      = 8
errorCode FMRPacketOverflow = 9
errorCode FMRError          = 10
errorCode Endpoint          = 11
errorCode USB               = 12
errorCode Communication     = 13
errorCode Socket            = 14
errorCode Module            = 15
errorCode Resolution        = 16
errorCode NoString          = 17
errorCode Checksum          = 18
errorCode Name              = 19
errorCode Configuration     = 20
errorCode Acknowledge       = 21
errorCode Unknown           = maxBound

codeError :: Word16 -> FlipperError
codeError 0  = OK
codeError 1  = MemAllocFailed
codeError 2  = Null
codeError 3  = Overflow
codeError 4  = NoDevice
codeError 5  = NotAttached
codeError 6  = AlreadyAttached
codeError 7  = FileAlreadyExists
codeError 8  = FileNotFound
codeError 9  = FMRPacketOverflow
codeError 10 = FMRError
codeError 11 = Endpoint
codeError 12 = USB
codeError 13 = Communication
codeError 14 = Socket
codeError 15 = Module
codeError 16 = Resolution
codeError 17 = NoString
codeError 18 = Checksum
codeError 19 = Name
codeError 20 = Configuration
codeError 21 = Acknowledge
codeError _  = Unknown

configure :: IO Bool
configure = retSuc <$> c_error_configure

pause :: IO ()
pause = c_error_pause

resume :: IO ()
resume = c_error_resume

raise :: FlipperError -> IO ()
raise e = c_error_raise (errorCode e) nullPtr

get :: IO FlipperError
get = codeError <$> c_error_get

clear :: IO ()
clear = c_lf_error_clear

foreign import ccall safe "flipper/error/error.h lf_error_configure"
    c_error_configure :: IO Word32

foreign import ccall safe "flipper/error/error.h lf_error_resume"
    c_error_resume :: IO ()

foreign import ccall safe "flipper/error/error.h lf_error_pause"
    c_error_pause :: IO ()

foreign import ccall safe "flipper/error/error.h lf_error_raise"
    c_error_raise :: Word16 -> CString -> IO ()

foreign import ccall safe "flipper/error/error.h error_get"
    c_error_get :: IO Word16

foreign import ccall safe "flipper/error/error.h lf_error_clear"
    c_lf_error_clear :: IO ()
