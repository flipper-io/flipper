module Flipper.Internal.FS where

import Data.Word

newtype FSHandle = FSHandle { unFSHandle :: Word32 }
