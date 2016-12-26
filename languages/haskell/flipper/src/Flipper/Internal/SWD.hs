{-|
Module      : Flipper.Internal.SWD
Description : Internal SWD Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.SWD (
    configure
  ) where

import Data.Word

import Flipper.Internal.Utils

configure :: IO Bool
configure = retSuc <$> c_swd_configure

foreign import ccall safe "flipper/swd/swd.h swd_configure"
    c_swd_configure :: IO Word32
