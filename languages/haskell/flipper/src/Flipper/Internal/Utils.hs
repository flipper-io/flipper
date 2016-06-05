{-|
Module      : Flipper.Internal.Utils
Description : Internal Utility Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.Utils (
    retSuc
  ) where

import qualified Foreign.Marshal.Utils as U

-- | Map the return value from a C function that returns non-zero on failure to
--   a 'Bool'.
retSuc :: (Eq a, Num a) => a -> Bool
retSuc = not . U.toBool
