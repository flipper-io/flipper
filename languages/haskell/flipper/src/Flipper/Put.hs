module Flipper.Put where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

import Data.Word

import Flipper.Buffer

import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

data Put = Put (Ptr Word8 -> IO ()) -- Buffer writer.
               Int                  -- Number of bytes contributed.

instance Monoid Put where
    mempty = Put (\_ -> return ()) 0
    mappend (Put w1 l1) (Put w2 l2) = Put w' (l1 + l2)
        where w' p = w1 p >> w2 (p `plusPtr` l1)

runPut :: Put -> IO Buffer
runPut (Put w l) = do b@(Buffer p _ _) <- allocBuffer l
                      withForeignPtr p w
                      return b

putBuffer :: Buffer -> Put
putBuffer (Buffer p o l) = Put w l
    where w d = withForeignPtr p $ \p' -> copyBytes (castPtr d) (castPtr p') l

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

-- | Uses ASCII encoding. This forces the entire string into memory. Must not
--   contain NULL.
putString :: String -> Put
putString = putBufferC . fromByteString . C.pack

-- | Uses UTF8 encoding. Must not contain NULL.
putText :: T.Text -> Put
putText = putBufferC . fromByteString . T.encodeUtf8

-- | May contain NULL. Must be less than 2^32 bytes long.
putByteString :: B.ByteString -> Put
putByteString = putBufferPascal . fromByteString
