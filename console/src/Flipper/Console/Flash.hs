module Flipper.Console.Flash where

import Control.Monad.Trans.Class

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS

import Data.Word

import Flipper.Console.FC

import Foreign.C.Types
import Foreign.Ptr

import System.Console.Haskeline

execFlash :: FilePath -> FC ()
execFlash fp = do
    f <- liftFC $ BS.readFile fp
    r <- liftFC $ BS.unsafeUseAsCStringLen f (\(p, s) -> c_flash p (fromIntegral s) nullPtr)
    case r of 0 -> pure ()
              _ -> lift (outputStrLn "slain")

foreign import ccall safe "flash"
    c_flash :: Ptr CChar -> Word64 -> Ptr Int -> IO Int
