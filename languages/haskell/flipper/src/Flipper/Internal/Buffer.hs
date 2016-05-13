{-|
Module      : Flipper.Internal.Buffer
Description : Internal Buffer Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

{-# LANGUAGE BangPatterns #-}

module Flipper.Internal.Buffer (
    Buffer(..)
  , emptyBuffer
  , allocBufferSafe
  , allocBuffer
  , toByteString
  , fromByteString
  , append
  ) where

import qualified Data.ByteString.Internal as B

import Data.Word

import GHC.ForeignPtr

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils

import System.IO.Unsafe

-- | A binary buffer.
data Buffer = Buffer !(ForeignPtr Word8) -- Pointer
                     !Int                -- Offset
                     !Int                -- Length

-- Just use the strict 'ByteString' implementation.
instance Eq Buffer where
    a == b = (toByteString a) == (toByteString b)

-- Just use the strict 'ByteString' implementation.
instance Ord Buffer where
    compare a b = compare (toByteString a) (toByteString b)

-- Just use the strict 'ByteString' implementation.
instance Show Buffer where
    show = show . toByteString

-- | The empty buffer.
emptyBuffer :: Buffer
emptyBuffer = Buffer (error "nullForeignPtr") 0 0

allocBufferSafe :: Int -> IO Buffer
allocBufferSafe l
    | l <= 0    = error "allocBuffer: length must be greater than zero."
    | otherwise = (\p -> Buffer (castForeignPtr p) 0 l) <$> mallocPlainForeignPtrBytes l

allocBuffer :: Int -> Buffer
allocBuffer = unsafePerformIO . allocBufferSafe

-- | O(1) conversion to a 'B.ByteString'.
toByteString :: Buffer -> B.ByteString
toByteString (Buffer p o l) = B.PS p o l

fromByteString :: B.ByteString -> Buffer
fromByteString (B.PS p o l) = Buffer p o l

-- | O(n+m) buffer concatenation.
append :: Buffer -> Buffer -> Buffer
append (Buffer _ _ 0) b = b
append a (Buffer _ _ 0) = a
append (Buffer p1 o1 l1) (Buffer p2 o2 l2) =
    let s                 = l1 + l2
        b@(Buffer p3 _ _) = allocBuffer s
    in unsafeDupablePerformIO $ withForeignPtr p1 $ \p1' ->
            withForeignPtr p2 $ \p2' ->
            withForeignPtr p3 $ \p3' ->
            do copyBytes p3' (plusPtr p1' o1) l1
               copyBytes (plusPtr p3' l1) (plusPtr p2' o1) l2
               return b
