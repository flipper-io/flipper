{-|
Module      : Flipper.Error
Description : Flipper error reporting.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

{-# LANGUAGE DeriveAnyClass #-}

module Flipper.Error (
    FlipperError(..)
  , FlipperException(..)
  ) where

import Control.Exception

import Data.Typeable

import Flipper.Internal.Error

data FlipperException = FlipperException FlipperError
                      deriving (Eq, Show, Typeable)

instance Exception FlipperException
