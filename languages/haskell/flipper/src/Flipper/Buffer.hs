{-# LANGUAGE BangPatterns #-}

module Flipper.Buffer where

import qualified Data.ByteString.Internal as B

import Data.Word

import GHC.ForeignPtr

import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.Unsafe

data Buffer = Buffer !(ForeignPtr Word8) -- Pointer
                     !Int                -- Offset
                     !Int                -- Length

allocBuffer :: Int -> IO Buffer
allocBuffer l
    | l <= 0    = error "allocBuffer: length must be greater than zero."
    | otherwise = (\p -> Buffer (castForeignPtr p) 0 l) <$> mallocPlainForeignPtrBytes l

-- 'Buffer's are pinned memory.
unsafeAllocBuffer :: Int -> Buffer
unsafeAllocBuffer l
    | l <= 0    = error "unsafeAllocBuffer: length must be greater than zero."
    | otherwise = (\p -> Buffer (castForeignPtr p) 0 l)
                  (unsafeDupablePerformIO (mallocPlainForeignPtrBytes l))

toByteString :: Buffer -> B.ByteString
toByteString (Buffer p o l) = B.PS p o l

fromByteString :: B.ByteString -> Buffer
fromByteString (B.PS p o l) = Buffer p o l