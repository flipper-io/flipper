{-|
Module      : Flipper.Buffer
Description : Binary buffer handling.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides a data type similar in principle to 'B.ByteString' but with
different allocation behavior and an interface specialized for building device
communication primitives.
-}

module Flipper.Buffer (
    Buffer()
  , emptyBuffer
  , toByteString
  , fromByteString
  , append
  ) where

import Flipper.Internal.Buffer
