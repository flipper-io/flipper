{-|
Module      : Flipper.Distribution.Binding
Description : Flipper Packages
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides types and parsers for handling external language bindings.

When a package is installed on a Flipper device, the symbols exposed in the
package's modules are available via FMR. However, it is often advantageous to
provide supplementary host-side code for interacting with the package's modules.
FPM is not opinionated when it comes to how this host-side code is implemented,
built, or distributed. However, to make using Flipper packages easier, FPM knows
how to interact with a few languages' package management tools and systems. If
the host-side code is implemented in a language whose package management system
is supported by FPM, package consumers can easily fetch and build the host-side
bindings. For a list of supported languages see the
"Flipper.Distribution.Language" module documentation.

FPM manifest files may contain as many unique langauge binding sections as
required. A binding section in a manifest file looks like this:

> binding <language>
> source: <source>

Where @\<language\>@ is one of the supported languages in
"Flipper.Distribution.Language", or any other language name; and @\<source\>@ is
a package identifier appropriate for the language's package management system,
or a URI for a repository that @git@ knows how to fetch.
-}

{-# LANGUAGE DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           , OverloadedStrings
           #-}

module Flipper.Distribution.Binding (
    Binding(..)
  , manifestBindings
  ) where

import Control.DeepSeq

import Data.Binary

import Data.Data

import Flipper.Distribution.Language
import Flipper.Distribution.Manifest

import GHC.Generics

-- | A language binding.
data Binding = Binding {
    -- | Binding language.
    bindingLang   :: Language
    -- | Binding source.
  , bindingSource :: LangSrc
  } deriving ( Eq
             , Ord
             , Show
             , Data
             , Typeable
             , Generic
             , NFData
             , Binary
             )

manifestBindings :: ManifestP [Binding]
manifestBindings = procBindingSections mkBinding
    where mkBinding l = Binding l <$>
                        (bindingKey l "source" >>= liftParser (parseLangSrc l))
