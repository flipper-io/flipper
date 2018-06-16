{-|
Module      : Flipper.Bufferable
Description : Serialization/Deserialization typeclass.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides the 'Bufferable' typeclass, representing any data that may
be sent to or received from the device over an arbitrary bus. This is
accomplished by providing a 'Put' serializer and a 'Get' deserializer. While
serializers and deserializers may be written from scratch, often the Haskell
data type representing the data being exchanged is analogous to the
corresponding C struct. When this is the case, a 'Bufferable' instance may be
derived automatically with GHC generics. For example:

> -- Needs -XDeriveGeneric, -XDeriveAnyClass
>
> import GHC.Generics
>
> import qualified Data.ByteString as B
>
> import Flipper.Bufferable
>
> data SomeStruct = SomeStruct {
>     ssID      :: Int
>   , ssName    :: String
>   , ssPayload :: B.ByteString
>   } deriving (Generic, Bufferable)

The following conditions must be satisfied for a 'Bufferable' instance to be
automatically generated:

 - The data type must have exactly one constructor.
 - The data type's constructor's arguments' types must have 'Bufferable'
   instances.
 - The data type's constructor must not be recursive.

== Struct Packing

The machinery for deriving 'Bufferable' instances assumes that the analogous C
struct's fields are arranged in the same order as the Haskell data type
constructor's arguments. However, no assumptions are made about how the struct
is packed. If possible, use @\_\_attribute\_\_((\_\_packed\_\_))@ on the
corresponding struct, and use fundamental Haskell types from "Data.Int",
"Data.Word", and "Foreign.C.Types" whose widths are guaranteed.

Even if the use of a padded struct unavoidable, it is still not necessary to
write a serializer/deserializer pair by hand. The 'Padding' type may be used in
Haskell data types to indicate where padding is present in the corresponding C
struct and how wide it is. Suppose we're dealing with the following C struct on
a machine with 32-bit words (like Flipper):

> typedef struct _rec
> {
>         uint8_t   a;
>         uint16_t b;
>         uint8_t  c;
>         uint32_t d;
> } rec;

If this struct is not packed, it will look like this in memory:

> ┌───────────┬─────────────────┬────────────┬───────────┬──────────────────┬────────────┐
> │8 bits of a│8 bits of padding│16 bits of b│8 bits of c│24 bits of padding│32 bits of d│
> └───────────┴─────────────────┴────────────┴───────────┴──────────────────┴────────────┘

This struct may be represented with the following Haskell data type:

> -- Needs -XDeriveGeneric, -XDeriveAnyClass, -XDataKinds
>
> import Data.Word
>
> import GHC.Generics
>
> import Flipper.Bufferable
>
> data Rec = Rec {
>    a    :: Word8
>  , pad1 :: Padding 1
>  , b    :: Word16
>  , c    :: Word8
>  , pad2 :: Padding 3
>  , d    :: Word32
>  } deriving (Generic, Bufferable)

== C Arrays

In C there are two common strategies for delimiting arrays. The first involves
a distinguished sentinel element whose presence indicates the end of the array;
C strings are an example of this strategy. To accomodate such structures the
'SentinelSequence' data type and accompanying 'Sentinel' typeclass are provided.
A value of type @SentinelSequence a s@ will generate a 'Bufferable' instance
that reads consecutive values of type @a@ until the 'sentinel' value of type @s@
is encountered. Analogously the generated serializer will encode adjacent values
of type @a@, followed by the 'sentinel' value of type @s@. An example for C
strings:

> newtype CString = CString { unCString :: SentinelSequence Char Char }
>                 deriving (Generic, Bufferable)
>
> instance Sentinel Char where
>     sentinel = '\0'

This example is redundant; the 'Bufferable' instance for 'String' already
provides this exact behavior, and the 'CBlock' 'Bufferable' instance provides
this behavior for 'B.ByteStrings'.

Another technique is to lead the array with an unsigned integer providing the
number of elements in the array. Applied to character data, such a structure is
known as a Pascal string. The 'SizedSequence' data type provides 'Bufferable'
instances for such structures. A value of type @SizedSequence a l@ will first
read a quantity of type @l@, and then read @l@ following elements. Here @l@ is a
phantom type that merely encodes what sort of integer is present at the
beginning of the array. Analogously the generated serializer will encode the
length of the sequence as type @l@, then encode adjacent values of type @a@. An
example for 32-bit led Pascal strings:

> newtype PString = PString { unPascalString :: SizedSequence Char Word32 }
>                 deriving (Generic, Bufferable)

The related type 'SizedByteString' provides the same behavior for
'B.ByteStrings'.

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
           , GeneralizedNewtypeDeriving
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
  , Padding()
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad

import Data.Complex
import Data.Data
import Data.Int
import Data.Ix
import Data.Monoid
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

-- | Generic class for 'Bufferable'.
class GBufferable g where
    gput :: g p -> Put
    gget :: Get (g p)

-- | Instance for uninhabited types.
instance GBufferable V1 where
    gput = undefined
    gget = undefined

-- | Instance for concretized types.
instance (Bufferable a) => GBufferable (K1 i a) where
    gput (K1 x) = put x
    gget        = K1 <$> get

-- | Instance for sum types.
instance (GBufferable f, GBufferable g) => GBufferable (f :*: g) where
    gput (f :*: g) = gput f <> gput g
    gget           = (:*:) <$> gget <*> gget

-- | Instance for metadata.
instance GBufferable a => GBufferable (M1 i t a) where
    gput (M1 x) = gput x
    gget        = M1 <$> gget

-- | Provides a 'Bufferable' instance whose deserializer sequentially decodes
--   values of type @a@ until the 'sentinel' value of type @s@ is encoded, where
--   'sentinel' is provided by the 'Sentinel' class. Analogously the generated
--   serializer will sequentially encode values of type @a@, followed by the
--   'sentinel' value of type @s@. @s@ is a phantom type, merely encoding the
--   type whose 'Sentinel' instance provides the 'sentinel' for values of this
--   type.
newtype SentinelSequence a s = SentinelSequence { unSentinelSequence :: [a] }
                             deriving ( Eq
                                      , Ord
                                      , Read
                                      , Show
                                      , Functor
                                      , Monoid
                                      , Foldable
                                      , Traversable
                                      )

-- | The class of types containing a single 'sentinel' value.
class Sentinel s where
    -- | The sentinel value.
    sentinel :: s

-- | Provides a 'Bufferable' instance whose deserializer first decodes an
--   integer of type @l@, then decodes @l@ values of type @a@. Analogously the
--   generated serializer will first encode the length of the sequence as type
--   @l@, then sequentially encode values of type @a@. @l@ is a phantom type,
--   merely encoding the width and interpretation of the integer leading the
--   array.
newtype SizedSequence a l = SizedSequence { unSizedSequence :: [a] }
                          deriving ( Eq
                                   , Ord
                                   , Read
                                   , Show
                                   , Functor
                                   , Monoid
                                   , Foldable
                                   , Traversable
                                   )

-- | Provides a 'Bufferable' instance for C strings, i.e. a contiguous block of
--   non-null bytes terminted by the null byte.
newtype CBlock = CBlock { unCBlock :: B.ByteString }
               deriving ( Eq
                        , Ord
                        , Read
                        , Show
                        , Generic
                        , NFData
                        , Monoid
                        )

-- | Provides a 'Bufferable' instance for arrays of bytes accompanied by a
--   leading integer of type @l@ indicating the number of bytes in the array.
newtype SizedByteString l = SizedByteString { unSizedByteString :: B.ByteString }
                          deriving ( Eq
                                   , Ord
                                   , Read
                                   , Show
                                   , Generic
                                   , NFData
                                   , Monoid
                                   )

-- | A type for encoding struct padding at the Haskell type level. Use of this
--   type requires a recent GHC supporting the @-XDataKinds@ language extension.
--   The type level natural @s@ is the number of bytes of padding provided by
--   the generated 'Bufferable' instance. Although this type is uninhabited
--   (i.e. 'undefined' or ⊥ is the only value of any 'Padding' type), degenerate
--   instances are provided for common typeclasses so that they may be derived
--   for data types containing padding.
data Padding (s :: Nat) deriving (Generic)

instance NFData (Padding s)

-- | '(==)' always returns 'True'.
instance Eq (Padding s) where
    _ == _ = True

-- | 'compare' always returns 'EQ'.
instance Ord (Padding s) where
    compare _ _ = EQ

-- | 'readPrec' consumes no input and returns 'undefined'.
instance Read (Padding s) where
    readPrec = pure undefined

-- | Outputs "Padding s" where s is a type level natural.
instance KnownNat s => Show (Padding s) where
    show p = "Padding " ++ show (padSize p)

-- | 'toEnum' returns 'undefined', 'fromEnum' returns 0.
instance Enum (Padding s) where
    toEnum _   = undefined
    fromEnum _ = 0

-- | 'minBound' = 'maxBound' = 0
instance Bounded (Padding s) where
    minBound = undefined
    maxBound = undefined

-- | 'range' = [], 'index' = 0, 'inRange' = True
instance Ix (Padding s) where
    range _     = []
    index _ _   = 0
    inRange _ _ = True

-- | Returns a 'Proxy' for the padding width.
padProxy :: Padding s -> Proxy s
padProxy _ = Proxy

-- | Provides the padding width at the term level.
padSize :: KnownNat s => Padding s -> Integer
padSize = natVal . padProxy

-- | The class of types for which 'Put' serializers and 'Get' deserializers are
--   available. Instances may be derived with GHC generics or written by hand
--   with the utilities in the "Flipper.Put" and "Flipper.Get" modules.
class Bufferable a where
    put :: a -> Put -- ^ Serializer.
    get :: Get a    -- ^ Deserializer.
    default put :: (Generic a, GBufferable (Rep a)) => a -> Put
    put = gput . from
    default get :: (Generic a, GBufferable (Rep a)) => Get a
    get = to <$> gget

instance ( Bufferable a
         , Bufferable s
         , Sentinel s
         ) => Bufferable (SentinelSequence a s) where
    put (SentinelSequence as) = mconcat (map put as) <> put (sentinel :: s)
    get = SentinelSequence <$> g
        where g = (Nothing <$ (get :: Get s)) <|> (Just <$> (get :: Get a))
                  >>= \case Nothing  -> return []
                            (Just v) -> (v:) <$> g

instance ( Bufferable a
         , Integral l
         , Num l
         , Bufferable l
         ) => Bufferable (SizedSequence a l) where
    put (SizedSequence as) = put (fromIntegral (length as) :: l)
                          <> mconcat (map put as)
    get = SizedSequence <$> ( (get :: Get l) >>= (\s ->
                              replicateM (fromIntegral s) (get :: Get a))
                            )

instance Bufferable CBlock where
    put = putBufferC . fromByteString . unCBlock
    get = (CBlock . B.pack) <$> getCBlock

instance ( Bufferable l
         , Integral l
         , Num l
         ) => Bufferable (SizedByteString l) where
    put (SizedByteString bs) = put (fromIntegral (B.length bs) :: l)
                            <> putBuffer (fromByteString bs)
    get = SizedByteString <$> ( (get :: Get l) >>=
                                (getSizedByteString . fromIntegral)
                              )

instance KnownNat p => Bufferable (Padding p) where
    put p = mconcat (replicate (fromIntegral (padSize p)) (putWord8 0))
    get = getSizedBlock (fromIntegral (padSize (undefined :: Padding p)))
       *> pure undefined

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

-- | Assumes ASCII encoding and the presence of a null terminator.
instance Bufferable String where
    put = putString
    get = getString

-- | Assumes UTF8 encoding and the presence of a null terminator.
instance Bufferable T.Text where
    put = putText
    get = getText

-- | Assumes the initial 4 bytes are an unsigned 32-bit little-endian length.
--   Identical to 'SizedByteString Word32'.
instance Bufferable B.ByteString where
    put = putByteString
    get = getByteString
