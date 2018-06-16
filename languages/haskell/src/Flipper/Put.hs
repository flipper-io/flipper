{-|
Module      : Flipper.Put
Description : Lightweight serialization monoid for data exchange.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides a serialization monoid intended for encoding C-style
structures sent to the device over an arbitrary bus. Suppose a Haskell program
will send the records of the following type to a program running on the
device:

>    data SomeStruct = SomeStruct {
>        ssId      :: Int
>      , ssName    :: String
>      , ssPayload :: ByteString
>      }

The C program on the device might use this corresponding struct:

>    typedef struct _some_struct
>    {
>        uint32_t id;
>        char *name;
>        uint32_t len;
>        void *payload;
>    } some_struct;

The following serializer may be defined:

>    putSomeStruct :: Put SomeStruct
>    putSomeStruct (SomeStruct i n p) = mconcat [ putWord32 (fromIntegral i)
>                                               , putString n
>                                               , putByteString p
>                                               ]

If a 'Get' deserializer is also defined, then @SomeStruct@ may have a
'Bufferable' instance, so bus interfaces can be used without
serialization/deserialization boilerplate.

This module, like all others in this package, is intended to be imported
@qualified@, e.g.

> import qualified Flipper.Put as Put
-}

module Flipper.Put (
    -- * The 'Put' Serialization Monoid
    Put()
    -- * Running Serializers
  , sizePut
  , runPut
    -- * Built-In Serializers
  , putBuffer
  , putBufferC
  , putBufferPascal
  , putStorable
  , putWord8
  , putInt8
  , putWord16
  , putInt16
  , putWord32
  , putInt32
  , putWord64
  , putInt64
  , putString
  , putText
  , putByteString
  ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import Data.Monoid

import Data.Int
import Data.Word

import Flipper.Internal.Buffer

import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

-- | A monoid for efficient 'Buffer' construction.
data Put = Put (Ptr Word8 -> IO ()) -- Buffer writer.
               Int                  -- Number of bytes contributed.

instance Monoid Put where
    mempty = Put (\_ -> return ()) 0
    mappend (Put w1 l1) (Put w2 l2) = Put w' (l1 + l2)
        where w' p = w1 p >> w2 (p `plusPtr` l1)

-- | Compute the number of bytes a serializer will contribute to a 'Buffer'.
sizePut :: Put -> Int
sizePut (Put _ l) = l

-- | Run a serializer, producing a 'Buffer'.
runPut :: Put -> Buffer
runPut (Put w l) = unsafePerformIO run
    where run = do b@(Buffer p _ _) <- allocBufferSafe l
                   withForeignPtr p w
                   return b

-- | Write out raw 'Buffer' contents.
putBuffer :: Buffer -> Put
putBuffer (Buffer p o l) = Put w l
    where w d = withForeignPtr p $ \p' -> copyBytes d (p' `plusPtr` o) l

-- | 'Put' a 'Buffer' like C would, with a null terminator.
putBufferC :: Buffer -> Put
putBufferC = (<> putStorable (0 :: Word8)) . putBuffer

-- | 'Put' a 'Buffer' like Pascal would, with a preceeding 32-bit word
--   indicating the length. The buffer lenght must not exceed 2^32 - 1.
putBufferPascal :: Buffer -> Put
putBufferPascal b@(Buffer _ _ l) = putStorable (fromIntegral l :: Word32)
                                <> putBuffer b

-- | 'Put' for any natively marshalable data.
putStorable :: Storable a => a -> Put
putStorable s = Put (\p -> poke (castPtr p) s) (sizeOf s)

-- | Serialize a 'Word8'.
putWord8 :: Word8 -> Put
putWord8 = putStorable

-- | Serialize a 'Int8'.
putInt8 :: Int8 -> Put
putInt8 = putStorable

-- | Serialize a 'Word16'.
putWord16 :: Word16 -> Put
putWord16 = putStorable

-- | Serialize a 'Int16'.
putInt16 :: Int16 -> Put
putInt16 = putStorable

-- | Serialize a 'Word32'.
putWord32 :: Word32 -> Put
putWord32 = putStorable

-- | Serialize a 'Int32'.
putInt32 :: Int32 -> Put
putInt32 = putStorable

-- | Serialize a 'Word64'.
putWord64 :: Word64 -> Put
putWord64 = putStorable

-- | Serialize a 'Int64'.
putInt64 :: Int64 -> Put
putInt64 = putStorable

-- | Uses ASCII encoding (or rather ASCII truncation). This forces the entire
--   string into memory. Must not contain NULL.
putString :: String -> Put
putString = putBufferC . fromByteString . C.pack

-- | Uses UTF8 encoding. Must not contain NULL.
putText :: T.Text -> Put
putText = putBufferC . fromByteString . T.encodeUtf8

-- | May contain NULL. Must be less than 2^32 - 1 bytes long.
putByteString :: B.ByteString -> Put
putByteString = putBufferPascal . fromByteString
