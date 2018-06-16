{-|
Module      : Flipper.Internal.Button
Description : Internal Button Module
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

module Flipper.Internal.Button (
    configure
  , read
  ) where

import Prelude hiding (read)

import Data.Word

import Flipper.Internal.Utils

configure :: IO Bool
configure = retSuc <$> c_button_configure

read :: IO Bool
read = retSuc <$> c_button_read

foreign import ccall safe "flipper/button/button.h button_configure"
    c_button_configure :: IO Word32

foreign import ccall safe "flipper/button/button.h button_read"
    c_button_read :: IO Word8
