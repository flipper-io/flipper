{-|
Module      : Flipper.Distribution.Language
Description : Flipper Packages
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides tools for working with other language's packaging tools.
-}

{-# LANGUAGE DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           #-}

module Flipper.Distribution.Language where

import Control.DeepSeq

import Data.Binary

import Data.Data

import qualified Data.Text as T

import GHC.Generics

-- | A binding language. FPM knows how to install (or at least fetch) these on
--   the user's behalf.
data Language = -- | FPM can fetch C bindings with Git.
                C
                -- | FPM can fetch CL bindings with Git.
              | CommonLisp
                -- | FPM can fetch C++ bindings with Git.
              | CPP
                -- | Research how C# people do things.
              | CS
                -- | Research how D people do things.
              | D
                -- | FPM can install Haskell bindings from Hackage.
              | Haskell
                -- | Research how Java people do things.
              | Java
                -- | FPM can install NodeJS bindings from NPM.
              | NodeJS
                -- | Research how ObjC people do things.
              | ObjC
                -- | FPM can install Perl bindings from CPAN.
              | Perl
                -- | FPM can install Python bindings from PyPI.
              | Python
                -- | FPM can install R bindings from CRAN.
              | R
                -- | Research how Ruby people do things.
              | Ruby
                -- | FPM can install Rust bindings with cargo.
              | Rust
                -- | Research how Scala people do things.
              | Scala
                -- | FPM can fetch Scheme bindings with Git.
              | Scheme
                -- | Research how Swift people do things.
              | Swift
                -- | FPM can fetch other bindings with Git.
              | Unknown T.Text
              deriving ( Eq
                       , Ord
                       , Read
                       , Show
                       , Data
                       , Typeable
                       , Generic
                       , NFData
                       , Binary
                       )

-- | A binding source. FPM know how to install (or at least fetch) from these
--   sources on the user's behalf.
data LangSrc = -- | A Haskell package on Hackage.
               Hackage T.Text
               -- | A NodeJS package on NPM.
             | NPM T.Text
               -- | A Perl package on CPAN.
             | CPAN T.Text
               -- | A Python package on PyPI.
             | PyPI T.Text
               -- | An R package on CRAN.
             | CRAN T.Text
               -- | A Rust cargo package.
             | Cargo T.Text
               -- | An SSH or HTTP URI for Git.
             | Git T.Text
             deriving ( Eq
                      , Ord
                      , Read
                      , Show
                      , Data
                      , Typeable
                      , Generic
                      , NFData
                      , Binary
                      )

-- | A language binding.
data Binding = Binding {
    -- | Binding language.
    bindingLang   :: Language
    -- | Binding source.
  , bindingSource :: LangSrc
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Data
             , Typeable
             , Generic
             , NFData
             , Binary
             )
