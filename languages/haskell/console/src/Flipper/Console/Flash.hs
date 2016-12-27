{-|
Module      : Flipper.Console.Flash
Description : Firmare flashing.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides access to a C routine for flashing firmware to the
ATSAM4S16B.
-}

module Flipper.Console.Flash where

import Control.Monad.Trans.Class

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS

import Data.Word

import Flipper.Console.FC

import Foreign.C.Types
import Foreign.Ptr

import System.Console.Haskeline

-- | Flash a firmware file to the device.
--   TODO: do some sanity checks on the file before flashing.
execFlash :: FilePath -> FC ()
execFlash fp = do
    f <- liftFC $ BS.readFile fp
    r <- liftFC $ BS.unsafeUseAsCStringLen f (\(p, s) -> c_flash p (fromIntegral s) nullPtr)
    case r of 0 -> pure ()
              _ -> lift (outputStrLn "slain")

foreign import ccall safe "flash"
    c_flash :: Ptr CChar -> Word64 -> Ptr Int -> IO Int
