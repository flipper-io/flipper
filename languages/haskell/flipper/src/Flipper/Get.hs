{-|
Module      : Flipper.Get
Description : Lightweight parser combinators for data exchange.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides a simple set of parser combinators intended for decoding
C-style structures received from the device over an arbitrary bus. Suppose a
program running on the device will send the following struct to a Haskell
program:

@
    typedef struct _some_struct
    {
        uint32_t id;
        char *name;
        uint32_t len;
        void *payload;
    } some_struct;
@

Where @name@ is assumed to be a null-terminated string, and @payload@ is an
array of bytes of length @len@. An analogous Haskell data type may be
declared:

@
    data SomeStruct = SomeStruct {
        ssId      :: Int
      , ssName    :: String
      , ssPayload :: ByteString
      }
@

As well as a parser:

@
    getSomeStruct :: Get SomeStruct
    getSomeStruct = do
        i <- fromIntegral <$> getWord32
        n <- getString
        l <- fromIntegral <$> getWord32
        p <- getSizedByteString l
        return $ SomeStruct i n p
@

Or using applicative style:

@
    getSomeStruct :: Get SomeStruct
    getSomeStruct = SomeStruct
                <*> (fromIntegral <$> getWord32)
                <*> getString
                <*> getByteString
@

If a 'Put' serializer is also defined, then @SomeStruct@ may have a 'Bufferable'
instance, so bus interfaces can be used without serialization/deserialization
boilerplate.
-}

{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Flipper.Get where

import Control.Applicative
import Control.Monad

import Data.Monoid

import Data.Char
import Data.Int
import Data.Word

import qualified Data.ByteString as B

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Flipper.Buffer

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

-- | A lightweight parser monad, intended for implementing decoders for C
--   structures sent to or received from the device over some bus.
newtype Get a = Get { unGet :: (Buffer -> Result a) }

-- | Parse result.
data Result a = -- | Successful parse.
                Done !Buffer a
              | -- | Request for more input.
                WantMore !Int (Get a)
              | -- | Parse failure.
                Failure !Buffer String

instance Functor Result where
    fmap f (Done b x)     = Done b (f x)
    fmap f (WantMore l g) = WantMore l (fmap f g)
    fmap _ (Failure b s)  = Failure b s

instance Functor Get where
    fmap f (Get g) = Get $ (fmap f) . g

instance Applicative Get where
    pure x              = Get (\b -> Done b x)
    (Get f) <*> (Get x) = Get $ \b -> case f b of (Done b' f')   -> fmap f' (x b')
                                                  (WantMore l c) -> WantMore l (c <*> (Get x))
                                                  (Failure b' e) -> Failure b' e

instance Alternative Get where
    empty               = fail "empty"
    (Get x) <|> (Get y) = Get $ \b -> case x b of (Done b' x')   -> Done b' x'
                                                  (WantMore l c) -> WantMore l (c <|> (Get y))
                                                  (Failure b' e) -> y b

instance Monad Get where
    return        = pure
    (Get x) >>= f = Get $ \b -> case x b of (Done b' x')   -> (unGet (f x')) b'
                                            (WantMore l c) -> WantMore l (c >>= f)
                                            (Failure b' e) -> Failure b' e
    fail e        = Get $ \b -> Failure b e

instance MonadPlus Get where
    mzero = fail "mzero"
    mplus = (<|>)

instance Monoid (Get a) where
    mempty = fail "mempty"
    mappend = (<|>)

-- | Run a 'Get' in a one-off fashion.
runGet :: Get a -> Buffer -> Either String a
runGet (Get g) = checkResult . g
    where checkResult (Done _ x)     = Right x
          checkResult (Failure _ e)  = Left e
          checkResult (WantMore _ _) = Left "runGet: parser requested more input."

-- | Run a 'Get' that may be supplied more input.
runGetWith :: Monad m => Get a -> (Int -> m Buffer) -> Buffer -> m (Either String a)
runGetWith (Get g) m i = case g i of (Done _ x)     -> return $ Right x
                                     (WantMore l c) -> m l >>= runGetWith (Get g) m
                                     (Failure _ e)  -> return $ Left e

-- | Generic parser for any type with a 'Storable' instance.
getStorable :: forall a . Storable a => Get a
getStorable = Get $ \b@(Buffer p o l) -> let s     = sizeOf (undefined :: a)
                                             val v = Done (Buffer p (o + s) (l - s)) v
                                         in if s > l then (WantMore (s - l) (Get ((unGet getStorable) . append b)))
                                                     else unsafeDupablePerformIO $ withForeignPtr p $ \p' ->
                                                             val <$> peek (plusPtr (castPtr p') o)

getWord8 :: Get Word8
getWord8 = getStorable

getInt8 :: Get Int8
getInt8 = getStorable

getWord16 :: Get Word16
getWord16 = getStorable

getInt16 :: Get Int16
getInt16 = getStorable

getWord32 :: Get Word32
getWord32 = getStorable

getInt32 :: Get Int32
getInt32 = getStorable

getInt64 :: Get Int64
getInt64 = getStorable

-- | Get a null-terminated (C-style) list of bytes, not including the NULL
--   terminator.
getCBlock :: Get [Word8]
getCBlock = Get g
    where g (Buffer _ _ 0) = WantMore 1 getCBlock
          g (Buffer p o l) = unsafeDupablePerformIO $ withForeignPtr p $ \p' ->
                do c <- peek (plusPtr p' o)
                   let s = sizeOf c
                   case c of 0 -> return $ Done (Buffer p (o + s) (l - s)) []
                             _ -> return $ (c:) <$> g (Buffer p (o + s) (l - s))

-- | Get a size-delimited block of bytes.
getSizedBlock :: Int -> Get Buffer
getSizedBlock s
    | s <= 0    = fail "getSizedBlock: size must be greater than zero."
    | otherwise = Get g
    where g b@(Buffer p o l)
            | l < s     = WantMore (s - l) (Get ((unGet (getSizedBlock s)) . append b))
            | otherwise = Done (Buffer p (o + s) (l - s)) (Buffer p o s)

-- | Assumes ASCII encoding and presence of NULL terminator.
getString :: Get String
getString = map (chr . fromIntegral) <$> getCBlock

-- | Assumes UTF8 encoding and presence of NULL terminator.
getText :: Get T.Text
getText = (T.decodeUtf8 . B.pack) <$> getCBlock

-- | Assumes initial 4 bytes are an unsigned 32-bit length.
getByteString :: Get B.ByteString
getByteString = do s <- getStorable :: Get Word32
                   toByteString <$> getSizedBlock (fromIntegral s)

getSizedByteString :: Int -> Get B.ByteString
getSizedByteString = (toByteString <$>) . getSizedBlock
