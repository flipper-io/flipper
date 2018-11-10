{-|
Module      : Flipper.Error
Description : Flipper error reporting.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

{-# LANGUAGE DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           #-}

module Flipper.Error (
    FlipperError(..)
  , FlipperException(..)
  ) where

import Control.DeepSeq

import Control.Exception

import Data.Typeable

import Flipper.Internal.Error

import GHC.Generics

-- | An exception providing a 'FlipperError'.
data FlipperException = FlipperException FlipperError
                      deriving ( Eq
                               , Ord
                               , Read
                               , Show
                               , Typeable
                               , Generic
                               , NFData
                               )

instance Exception FlipperException
