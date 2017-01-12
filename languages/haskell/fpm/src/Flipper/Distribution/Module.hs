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
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           #-}

module Flipper.Distribution.Module (
    Module(..)
  , manifestModules
  ) where

import Control.DeepSeq

import Data.Binary

import Data.Data

import Data.List.NonEmpty

import GHC.Generics

import Flipper.Distribution.Manifest
import Flipper.Distribution.Parser
import Flipper.Distribution.SymbolName

import qualified Text.Megaparsec as M

-- | A module.
data Module = Module {
    -- | Module name.
    modName    :: SymbolName
    -- | Exposed module symbols.
  , modSymbols :: (NonEmpty SymbolName)
    -- | Module entry points.
  , modEntries :: [SymbolName]
  } deriving ( Eq
             , Ord
             , Show
             , Data
             , Typeable
             , Generic
             )

-- TODO: remove this when Stackage LTS updates the binary package.
instance Binary (NonEmpty SymbolName)
instance NFData Module
instance Binary Module

manifestModules :: ManifestP (NonEmpty Module)
manifestModules = procModuleSections mkModule
    where mkModule m = Module m
                   <$> (moduleKey m "symbols" >>= liftParser someSymbs)
                   <*> (moduleKey m "entry-points" >>= liftParser symbs)
          ident     = lexed parseSymbolName
          symbs     = M.sepBy ident (symb ",")
          someSymbs = fromList <$> M.sepBy1 ident (symb ",")
