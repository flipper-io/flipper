{-|
Module      : Flipper.Distribution.Manifest
Description : Flipper Packages
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides the first-pass parser for @pkg.fpm@ files.
-}

{-# LANGUAGE DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           #-}

module Flipper.Distribution.Manifest where

import Control.DeepSeq

import Data.Binary

import Data.Data

import qualified Data.Map as M

import qualified Data.Text as T

import Flipper.Distribution.Language
import Flipper.Distribution.Module

import GHC.Generics

-- | Internal representation of @pkg.fpm@ files after the first parsing pass.
data Manifest = Manifest {
    -- | Header key-value pairs before any module or binding sections.
    header       :: M.Map T.Text T.Text
    -- | Module sections indexed by module names.
  , modSections  :: M.Map SymbolName (M.Map T.Text T.Text)
    -- | Binding sections indexed by language.
  , bindSections :: M.Map Language (M.Map T.Text T.Text)
  } deriving ( Eq
             , Ord
             , Show
             , Data
             , Typeable
             , Generic
             , NFData
             , Binary
             )
