{-# LANGUAGE BangPatterns #-}

module Flipper.Get where

import Flipper.Buffer

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

-- The parser operates on strict buffers, so no incremental interface is needed.
-- Refactor this to use CPS later.
newtype Get a = Get { unGet :: (Buffer -> Result a) }

data Result a = Done !Buffer a
              | Failure !Buffer String

runGet :: Get a -> Buffer -> Either String a
runGet (Get g) = checkResult . g
    where checkResult (Done _ x)    = Right x
          checkResult (Failure _ e) = Left e

getStorable :: Storable a => Get a
getStorable = Get g
    where g (Buffer p o l) = unsafeDupablePerformIO $ withForeignPtr p
            (\p' -> (\v -> Done (Buffer p (o + sizeOf v) (l - sizeOf v)) v)
                   <$> peek (castPtr p'))
