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
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
           #-}

module Flipper.Distribution.Package (
    PackageName()
  , parsePackageName
  , PackageID(..)
  , manifestPackageID
  , Dependency(..)
  , parseDependencies
  , manifestDependencies
  , PackageDescription(..)
  , manifestPackageDescription
  ) where

import Control.Applicative

import Control.DeepSeq

import Data.Binary

import Data.Data

import Data.List.NonEmpty

import qualified Data.Text as T

import Flipper.Distribution.Binding
import Flipper.Distribution.Language
import Flipper.Distribution.License
import Flipper.Distribution.Manifest
import Flipper.Distribution.Module
import Flipper.Distribution.Parser
import Flipper.Distribution.Version

import GHC.Generics

import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Text as M

-- | A legal package name is any string of non-whitespace non-control Unicode
--   characters.
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

parsePackageName :: M.Parser PackageName
parsePackageName = PackageName <$> word

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

manifestPackageID :: ManifestP PackageID
manifestPackageID = PackageID <$> name <*> version
    where name    = headerKey "name" >>= liftParser parsePackageName
          version = headerKey "version" >>= liftParser parseVersion

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

parseDependencies :: M.Parser [Dependency]
parseDependencies = M.sepBy1 parseDep (symb ",")
    where parseDep = Dependency <$> lexed parsePackageName
                                <*> lexed parseVersionRange

manifestDependencies :: ManifestP [Dependency]
manifestDependencies = headerKey "dependencies" >>= liftParser parseDependencies

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
    -- | Package dependencies as listed in the @pkg.fpm@ file, not to be
    --   confused with the package dependency closure computed by dependency
    --   resolution.
  , dependencies :: [Dependency]
    -- | Package specification version.
  , specVersion  :: Version
    -- | Exposed modules.
  , modules      :: NonEmpty Module
    -- | Language bindings.
  , bindings     :: [Binding]
  } deriving ( Eq
             , Ord
             , Show
             , Data
             , Typeable
             , Generic
             )

-- TODO: remove this when Stackage LTS updates the binary package.
instance Binary (NonEmpty Module)
instance NFData PackageDescription
instance Binary PackageDescription

manifestPackageDescription :: ManifestP PackageDescription
manifestPackageDescription =
    PackageDescription <$> manifestPackageID
                       <*> (headerKey "license" >>= liftParser parseLicense)
                       <*> (T.unpack <$> headerKey "license-file")
                       <*> headerKey "copyright"
                       <*> headerKey "maintainer"
                       <*> headerKey "author"
                       <*> headerKey "homepage"
                       <*> headerKey "bug-reports"
                       <*> headerKey "synopsis"
                       <*> manifestDependencies
                       <*> (headerKey "fpm-version" >>= liftParser parseVersion)
                       <*> manifestModules
                       <*> manifestBindings
