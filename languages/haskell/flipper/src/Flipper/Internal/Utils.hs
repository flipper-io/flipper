module Flipper.Internal.Utils where

import qualified Foreign.Marshal.Utils as U

retSuc :: (Eq a, Num a) => a -> Bool
retSuc = not . U.toBool
