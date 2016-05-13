{-|
Module      : Flipper.Internal.FDL
Description : Internal FDL Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.FDL (
    FDLKey()
  , FDLAddress()
  , load
  , launch
  , resolve
  ) where

import Data.Word

import Foreign.Ptr

newtype FDLKey = FDLKey { unFDLKey :: Word16 }
newtype FDLAddress = FDLAddress { unFDLAddress :: Ptr Word8 }

load :: FDLKey -> IO FDLAddress
load = fmap FDLAddress . c_fdl_load . unFDLKey

launch :: FDLKey -> IO ()
launch = c_fdl_launch . unFDLKey

resolve :: FDLKey -> FDLAddress -> IO ()
resolve (FDLKey k) (FDLAddress a) = c_fdl_resolve k a

foreign import ccall safe "flipper/fdl.hs fdl_load"
    c_fdl_load :: Word16 -> IO (Ptr Word8)

foreign import ccall safe "flipper/fdl.hs fdl_launch"
    c_fdl_launch :: Word16 -> IO ()

foreign import ccall safe "flipper/fdl.hs fdl_resolve"
    c_fdl_resolve :: Word16 -> Ptr Word8 -> IO ()
