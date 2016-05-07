{-|
Module      : Flipper.Bufferable
Description : Serialization/Deserialization typeclass.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides the 'Bufferable' typeclass, representing any data that may
be sent to or received from the device over an arbitrary bus.
-}

{-# LANGUAGE FlexibleInstances #-}

module Flipper.Bufferable where

import Data.Complex
import Data.Int
import Data.Ratio
import Data.Word

import Flipper.Get
import Flipper.Put

import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable

import System.Posix.Types

class Bufferable a where
    put :: a -> Put -- ^ Serializer.
    get :: Get a    -- ^ Deserializer.

instance Bufferable Bool where
    put = putStorable
    get = getStorable

instance Bufferable Char where
    put = putStorable
    get = getStorable

instance Bufferable Double where
    put = putStorable
    get = getStorable

instance Bufferable Float where
    put = putStorable
    get = getStorable

instance Bufferable Int where
    put = putStorable
    get = getStorable

instance Bufferable Int8 where
    put = putStorable
    get = getStorable

instance Bufferable Int16 where
    put = putStorable
    get = getStorable

instance Bufferable Int32 where
    put = putStorable
    get = getStorable

instance Bufferable Int64 where
    put = putStorable
    get = getStorable

instance Bufferable Word where
    put = putStorable
    get = getStorable

instance Bufferable Word8 where
    put = putStorable
    get = getStorable

instance Bufferable Word16 where
    put = putStorable
    get = getStorable

instance Bufferable Word32 where
    put = putStorable
    get = getStorable

instance Bufferable Word64 where
    put = putStorable
    get = getStorable

instance Bufferable CUIntMax where
    put = putStorable
    get = getStorable

instance Bufferable CIntMax where
    put = putStorable
    get = getStorable

instance Bufferable CUIntPtr where
    put = putStorable
    get = getStorable

instance Bufferable CSUSeconds where
    put = putStorable
    get = getStorable

instance Bufferable CUSeconds where
    put = putStorable
    get = getStorable

instance Bufferable CTime where
    put = putStorable
    get = getStorable

instance Bufferable CClock where
    put = putStorable
    get = getStorable

instance Bufferable CSigAtomic where
    put = putStorable
    get = getStorable

instance Bufferable CWchar where
    put = putStorable
    get = getStorable

instance Bufferable CSize where
    put = putStorable
    get = getStorable

instance Bufferable CPtrdiff where
    put = putStorable
    get = getStorable

instance Bufferable CDouble where
    put = putStorable
    get = getStorable

instance Bufferable CFloat where
    put = putStorable
    get = getStorable

instance Bufferable CULLong where
    put = putStorable
    get = getStorable

instance Bufferable CULong where
    put = putStorable
    get = getStorable

instance Bufferable CLong where
    put = putStorable
    get = getStorable

instance Bufferable CUInt where
    put = putStorable
    get = getStorable

instance Bufferable CInt where
    put = putStorable
    get = getStorable

instance Bufferable CUShort where
    put = putStorable
    get = getStorable

instance Bufferable CShort where
    put = putStorable
    get = getStorable

instance Bufferable CUChar where
    put = putStorable
    get = getStorable

instance Bufferable CSChar where
    put = putStorable
    get = getStorable

instance Bufferable CChar where
    put = putStorable
    get = getStorable

instance Bufferable IntPtr where
    put = putStorable
    get = getStorable

instance Bufferable WordPtr where
    put = putStorable
    get = getStorable

instance Bufferable Fd where
    put = putStorable
    get = getStorable

instance Bufferable CRLim where
    put = putStorable
    get = getStorable

instance Bufferable CTcflag where
    put = putStorable
    get = getStorable

instance Bufferable CSpeed where
    put = putStorable
    get = getStorable

instance Bufferable CCc where
    put = putStorable
    get = getStorable

instance Bufferable CUid where
    put = putStorable
    get = getStorable

instance Bufferable CNlink where
    put = putStorable
    get = getStorable

instance Bufferable CGid where
    put = putStorable
    get = getStorable

instance Bufferable CSsize where
    put = putStorable
    get = getStorable

instance Bufferable CPid where
    put = putStorable
    get = getStorable

instance Bufferable COff where
    put = putStorable
    get = getStorable

instance Bufferable CMode where
    put = putStorable
    get = getStorable

instance Bufferable CIno where
    put = putStorable
    get = getStorable

instance Bufferable CDev where
    put = putStorable
    get = getStorable

instance (Storable a, Integral a) => Bufferable (Ratio a) where
    put = putStorable
    get = getStorable

instance Bufferable (StablePtr a) where
    put = putStorable
    get = getStorable

instance Bufferable (Ptr a) where
    put = putStorable
    get = getStorable

instance Bufferable (FunPtr a) where
    put = putStorable
    get = getStorable

instance Storable a => Bufferable (Complex a) where
    put = putStorable
    get = getStorable
