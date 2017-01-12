{-|
Module      : Flipper.Distribution.Binding
Description : Flipper Packages
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
