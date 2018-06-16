{-|
Module      : Flipper.Internal.Flipper
Description : Internal Flipper Device Module
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

module Flipper.Internal.Flipper (
    Endpoint(..)
  , select
  , attach
  , detach
  ) where

import Control.DeepSeq

import Data.Data

import Flipper.Internal.Error (pause)
import Flipper.Internal.Utils

import Foreign.C.String
import Foreign.C.Types

import GHC.Generics

-- | An endpoint via which a Flipper device may be attached.
data Endpoint = USB (Maybe String)    -- ^ Local connection via USB.
              | Network String String -- ^ Remote connection via TCP/UDP.
              | FVM                   -- ^ Flipper simulation via FVM.
              deriving ( Eq
                       , Ord
                       , Read
                       , Show
                       , Data
                       , Typeable
                       , Generic
                       , NFData
                       )

select :: String -> IO Bool
select n = withCString n ((retSuc <$>) . c_flipper_select)

attach :: Endpoint -> IO Bool
attach   (USB Nothing) = pause >> withCString "flipper" ((retSuc <$>) . c_flipper_attach_usb)
attach (USB (Just n))  = pause >> withCString n ((retSuc <$>) . c_flipper_attach_usb)
attach (Network n h)   = pause >> withCString n (\n' ->
                                  withCString h (\h' ->
                                  retSuc <$> c_flipper_attach_network n' h'
                                  ))
attach FVM             = pause >> return False

detach :: String -> IO Bool
detach n = withCString n ((retSuc <$>) . c_flipper_detach)

foreign import ccall safe "flipper.h flipper_select"
    c_flipper_select :: CString -> IO CInt

foreign import ccall safe "flipper.h flipper_detach"
    c_flipper_detach :: CString -> IO CInt

foreign import ccall safe "flipper.h flipper_attach_usb"
    c_flipper_attach_usb :: CString -> IO CInt

foreign import ccall safe "flipper.h flipper_attach_network"
    c_flipper_attach_network :: CString -> CString -> IO CInt
