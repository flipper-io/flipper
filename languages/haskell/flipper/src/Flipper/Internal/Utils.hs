module Flipper.Internal.Utils where

import qualified Foreign.Marshal.Util as U

retSuc :: Num a => a -> Bool
retSuc = not . U.toBool
