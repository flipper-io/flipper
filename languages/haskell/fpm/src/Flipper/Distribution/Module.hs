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

module Flipper.Distribution.Module (
    Module(..)
  , manifestModules
  ) where

import Control.DeepSeq

import Data.Binary

import Data.Data

import GHC.Generics

import Flipper.Distribution.Manifest
import Flipper.Distribution.SymbolName

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

manifestModules :: ManifestP [Module]
manifestModules = undefined
