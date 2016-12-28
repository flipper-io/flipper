{-|
Module      : Flipper.Distribution.Module
Description : Flipper Packages
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides module specifications.
-}

{-# LANGUAGE DeriveDataTypeable
           , DeriveGeneric
           , GeneralizedNewtypeDeriving
           #-}

module Flipper.Distribution.Module where

import Control.DeepSeq

import Data.Binary

import Data.Data

import qualified Data.Text as T

import GHC.Generics

-- | A module symbol name. This must be a legal C symbol token.
newtype SymbolName = SymbolName { unSymbolName :: T.Text }
                   deriving ( Eq
                            , Ord
                            , Show
                            , Monoid
                            , Data
                            , Typeable
                            , NFData
                            , Binary
                            )

-- | A module.
data Module = Module {
    -- | Module name.
    modName    :: SymbolName
    -- | Exposed module symbols.
  , modSymbols :: [SymbolName]
    -- | Module entry points.
  , modEntries :: [SymbolName]
  } deriving ( Eq
             , Ord
             , Show
             , Data
             , Typeable
             , Generic
             )

instance NFData Module
instance Binary Module
