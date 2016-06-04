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

{-# LANGUAGE FlexibleInstances
           , TypeOperators
           , DataKinds
           , DefaultSignatures
           , KindSignatures
           , FlexibleContexts
           , LambdaCase
           , ScopedTypeVariables
           , DeriveFunctor
           , DeriveFoldable
           , DeriveGeneric
           , DeriveTraversable
           , DeriveDataTypeable
           , DeriveAnyClass
           #-}

module Flipper.Bufferable (
    -- * The 'Bufferable' Class
    Bufferable(..)
    -- * Types for Generic 'Bufferable' Instances
  , Sentinel(..)
  , SentinelSequence(..)
  , SizedSequence(..)
  , CBlock(..)
  , SizedByteString(..)
  , Padding(..)
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad

import Data.Complex
import Data.Data
import Data.Int
import Data.Ix
import Data.List
import Data.Monoid
import Data.Proxy
import Data.String
import Data.Ratio
import Data.Word

import qualified Data.ByteString as B
import qualified Data.Text       as T

import Flipper.Buffer
import Flipper.Get
import Flipper.Put

import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable

import GHC.Generics
import GHC.TypeLits

import System.Posix.Types

import Text.Read hiding (get)

class GBufferable g where
    gput :: g p -> Put
    gget :: Get (g p)

instance GBufferable V1 where
    gput = undefined
    gget = undefined

instance (Bufferable a) => GBufferable (K1 i a) where
    gput (K1 x) = put x
    gget        = K1 <$> get

instance (GBufferable f, GBufferable g) => GBufferable (f :*: g) where
    gput (f :*: g) = gput f <> gput g
    gget           = (:*:) <$> gget <*> gget

instance GBufferable a => GBufferable (M1 i t a) where
    gput (M1 x) = gput x
    gget        = M1 <$> gget

newtype SentinelSequence a s = SentinelSequence { unSentinelSequence :: [a] }
                             deriving ( Eq
                                      , Ord
                                      , Read
                                      , Show
                                      , Functor
                                      , Foldable
                                      , Traversable
                                      )

class Sentinel s where
    sentinel :: s

newtype SizedSequence a l = SizedSequence { unSizedSequence :: [a] }
                          deriving ( Eq
                                   , Ord
                                   , Read
                                   , Show
                                   , Functor
                                   , Foldable
                                   , Traversable
                                   )

newtype CBlock = CBlock { unCBlock :: B.ByteString }
               deriving ( Eq
                        , Ord
                        , Read
                        , Show
                        , Generic
                        , NFData
                        )

newtype SizedByteString l = SizedByteString { unSizedByteString :: B.ByteString }
                          deriving ( Eq
                                   , Ord
                                   , Read
                                   , Show
                                   , Generic
                                   , NFData
                                   )

data Padding (s :: Nat) deriving (Generic, NFData)

instance Eq (Padding s) where
    _ == _ = True

instance Ord (Padding s) where
    compare _ _ = EQ

instance Read (Padding s) where
    readPrec = pure undefined

instance KnownNat s => Show (Padding s) where
    show p = "Padding " ++ show (padSize p)

instance Enum (Padding s) where
    toEnum _   = undefined
    fromEnum _ = 0

instance Bounded (Padding s) where
    minBound = undefined
    maxBound = undefined

instance Ix (Padding s) where
    range _     = []
    index _ _   = 0
    inRange _ _ = True

padProxy :: Padding s -> Proxy s
padProxy _ = Proxy

padSize :: KnownNat s => Padding s -> Integer
padSize = natVal . padProxy

class Bufferable a where
    put :: a -> Put -- ^ Serializer.
    get :: Get a    -- ^ Deserializer.
    default put :: (Generic a, GBufferable (Rep a)) => a -> Put
    put = gput . from
    default get :: (Generic a, GBufferable (Rep a)) => Get a
    get = to <$> gget

instance (Bufferable a, Bufferable s, Sentinel s) => Bufferable (SentinelSequence a s) where
    put (SentinelSequence as) = (mconcat (map put as)) <> (put (sentinel :: s))
    get = SentinelSequence <$> g
        where g = (Nothing <$ (get :: Get s)) <|> (Just <$> (get :: Get a))
                  >>= \case Nothing  -> return []
                            (Just v) -> (v:) <$> g

instance (Bufferable a, Integral l, Num l, Bufferable l) => Bufferable (SizedSequence a l) where
    put (SizedSequence as) = put (fromIntegral (length as) :: l) <> mconcat (map put as)
    get = SizedSequence <$> ((get :: Get l) >>= (\s -> replicateM (fromIntegral s) (get :: Get a)))

instance Bufferable CBlock where
    put = putBufferC . fromByteString . unCBlock
    get = (CBlock . B.pack) <$> getCBlock

instance (Bufferable l, Integral l, Num l) => Bufferable (SizedByteString l) where
    put (SizedByteString bs) = put (fromIntegral (B.length bs) :: l) <> (putBuffer (fromByteString bs))
    get = SizedByteString <$> ((get :: Get l) >>= (getSizedByteString . fromIntegral))

instance KnownNat p => Bufferable (Padding p) where
    put p = mconcat (replicate (fromIntegral (padSize p)) (putWord8 0))
    get = getSizedBlock (fromIntegral (padSize (undefined :: Padding p))) *> pure undefined

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

instance Bufferable String where
    put = putString
    get = getString

instance Bufferable T.Text where
    put = putText
    get = getText

instance Bufferable B.ByteString where
    put = putByteString
    get = getByteString
