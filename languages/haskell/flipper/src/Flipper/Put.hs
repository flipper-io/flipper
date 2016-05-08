{-|
Module      : Flipper.Put
Description : Lightweight serialization monoid for data exchange.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides a serialization monad intended for encoding C-style
structures sent to the device over an arbitrary bus. Suppose a Haskell program
will send the records of the following type to a program running on the
device:

@
    data SomeStruct = SomeStruct {
        ssId      :: Int
      , ssName    :: String
      , ssPayload :: ByteString
      }
@

The C program on the device might use this corresponding struct:

@
    typedef struct _some_struct
    {
        uint32_t id;
        char *name;
        uint32_t len;
        void *payload;
    } some_struct;
@

The following serializer may be defined:

@
    putSomeStruct :: Put SomeStruct
    putSomeStruct (SomeStruct i n p) = mconcat [ putWord32 (fromIntegral i)
                                               , putString n
                                               , putByteString p
                                               ]
@

If a 'Get' deserializer is also defined, then @SomeStruct@ may have a 'Bufferable'
instance, so bus interfaces can be used without serialization/deserialization
boilerplate.
-}

module Flipper.Put where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import Data.Monoid

import Data.Int
import Data.Word

import Flipper.Buffer

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

runPut :: Put -> Buffer
runPut (Put w l) = unsafePerformIO run
    where run = do b@(Buffer p _ _) <- allocBufferSafe l
                   withForeignPtr p w
                   return b

putBuffer :: Buffer -> Put
putBuffer (Buffer p o l) = Put w l
    where w d = withForeignPtr p $ \p' -> copyBytes d p' l

-- | 'Put' a 'Buffer' like C would, with a null terminator.
putBufferC :: Buffer -> Put
putBufferC = (<> putStorable (0 :: Word8)) . putBuffer

-- | 'Put' a 'Buffer' like Pascal would, with a preceeding 32-bit word
--   indicating the length.
putBufferPascal :: Buffer -> Put
putBufferPascal b@(Buffer _ _ l) = putStorable ((fromIntegral l) :: Word32) <> putBuffer b

-- | 'Put' for any natively marshalable data.
putStorable :: Storable a => a -> Put
putStorable s = Put (\p -> poke (castPtr p) s) (sizeOf s)

putWord8 :: Word8 -> Put
putWord8 = putStorable

putInt8 :: Int8 -> Put
putInt8 = putStorable

putWord16 :: Word16 -> Put
putWord16 = putStorable

putInt16 :: Int16 -> Put
putInt16 = putStorable

putWord32 :: Word32 -> Put
putWord32 = putStorable

putInt32 :: Int32 -> Put
putInt32 = putStorable

putInt64 :: Int64 -> Put
putInt64 = putStorable

-- | Uses ASCII encoding (or rather ASCII truncation). This forces the entire
--   string into memory. Must not contain NULL.
putString :: String -> Put
putString = putBufferC . fromByteString . C.pack

-- | Uses UTF8 encoding. Must not contain NULL.
putText :: T.Text -> Put
putText = putBufferC . fromByteString . T.encodeUtf8

-- | May contain NULL. Must be less than 2^32 bytes long.
putByteString :: B.ByteString -> Put
putByteString = putBufferPascal . fromByteString
