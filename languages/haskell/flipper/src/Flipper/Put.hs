module Flipper.Put where

import Data.Word

import Flipper.Buffer

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

data Put = Put (Ptr Word8 -> IO ()) -- Buffer writer.
               Int                  -- Number of bytes contributed.

instance Monoid Put where
    mempty = Put (\_ -> return ()) 0
    mappend (Put w1 l1) (Put w2 l2) = Put w' (l1 + l2)
        where w' p = w1 p >> w2 (p `plusPtr` l1)

runPut :: Put -> Buffer
runPut (Put w l) = let b@(Buffer p _ _) = unsafeAllocBuffer l
                       r                = unsafeDupablePerformIO $ withForeignPtr p w
                   in seq r b

putStorable :: Storable a => a -> Put
putStorable s = Put (\p -> poke (castPtr p) s) (sizeOf s)
