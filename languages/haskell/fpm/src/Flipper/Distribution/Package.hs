{-|
Module      : Flipper.Distribution.Package
Description : Flipper Packages
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides package specifications.
-}

{-# LANGUAGE DeriveDataTypeable
           , DeriveGeneric
           , GeneralizedNewtypeDeriving
           #-}

module Flipper.Distribution.Package where

import Control.DeepSeq

import Data.Binary

import Data.Data

import qualified Data.Text as T

import Flipper.Distribution.Binding
import Flipper.Distribution.Language
import Flipper.Distribution.License
import Flipper.Distribution.Module
import Flipper.Distribution.Version

import GHC.Generics

-- | A legal package name is something I still need to think more about...
newtype PackageName = PackageName { unPackageName :: T.Text }
                    deriving ( Eq
                             , Ord
                             , Show
                             , Monoid
                             , Data
                             , Typeable
                             , NFData
                             , Binary
                             )

-- | A name and version uniquely identifies a package.
data PackageID = PackageID {
    -- | Package name.
    pkgName    :: PackageName
    -- | Package version.
  , pkgVersion :: Version
  } deriving ( Eq
             , Ord
             , Show
             , Data
             , Typeable
             , Generic
             )

instance NFData PackageID
instance Binary PackageID

-- | A name and version range identifies a package dependency.
data Dependency = Dependency {
    -- | Dependency package name.
    depName  :: PackageName
    -- | Dependency package version.
  , depRange :: VersionRange
  } deriving ( Eq
             , Ord
             , Show
             , Data
             , Typeable
             , Generic
             )

instance NFData Dependency
instance Binary Dependency

-- | The internal representation of a @pkg.fpm@ file. This includes metadata
--   such as the package name, version, description, and license, as well as
--   data required for building the package, such as the package dependencies,
--   exposed modules, and application entry points.
data PackageDescription = PackageDescription {
    -- | Package name and version.
    package      :: PackageID
    -- | Package software license type.
  , license      :: License
    -- | Package software license text.
  , licenseFile  :: FilePath
    -- | Package copyright holder.
  , copyright    :: T.Text
    -- | Package maintainer email.
  , maintainer   :: T.Text
    -- | Package author name.
  , author       :: T.Text
    -- | Package homepage URL.
  , homepage     :: T.Text
    -- | Package bug report URL.
  , bugReports   :: T.Text
    -- | One-line package summary.
  , synopsis     :: T.Text
    -- | Package description. This text is parsed as markdown by the Flipper
    --   package server and displayed on the fpm.flipper.io package page.
  , description  :: T.Text
    -- | Package dependencies as listed in the @pkg.fpm@ file, not to be
    --   confused with the package dependency closure computed by dependency
    --   resolution.
  , dependencies :: [Dependency]
    -- | Package specification version.
  , specVersion  :: Version
    -- | Exposed modules.
  , modules      :: [Module]
    -- | Language bindings.
  , bindings     :: [Binding]
  } deriving ( Eq
             , Ord
             , Show
             , Data
             , Typeable
             , Generic
             )

instance NFData PackageDescription
instance Binary PackageDescription
