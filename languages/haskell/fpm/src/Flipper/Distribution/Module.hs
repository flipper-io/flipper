{-|
Module      : Flipper.Distribution.Module
Description : Flipper Packages
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides types and parsers for handling Flipper package modules.

A Flipper package defines one or more Flipper modules. A module has a name, a
non-empty sequence of exported symbols, and zero or more entry points. Modules
with entry points may be executed as programs on the device. Module names,
exported symbols, and entry points must be legal C symbol names; see the
"Flipper.Distribution.SymbolName" module documentation for precisely what
constitutes a legal C symbol name.

FPM manifest files must contain at least one unique module section. A module
section in a manifest file looks like this:

> module <name>
> symbols: <symbs>
> entry-points: <entries>

Where @<name>@ is the module name, @<symbs> is a comma-separated list of
exported symbols, and @<entries>@ is a comma-separated list of entry points.
-}

{-# LANGUAGE DeriveDataTypeable
           , DeriveGeneric
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
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
