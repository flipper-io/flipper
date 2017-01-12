{-|
Module      : Flipper.Distribution.Package
Description : Flipper Packages
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides types and parsers for package specifications.

The 'PackageDescription' type documentation provides the detailed syntax and
semantics for each field.
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
--   characters. This is quite liberal and we may decide to add more
--   restrictions later on.
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

-- | A name and version range identifies a package dependency. In a manifest
--   file, dependencies are listed in a comma-separated list, with each element
--   consisting of a 'PackageName', followed by whitespace, followed by a
--   'VersionRange'. For example:
--
-- > dependencies: packageX == 1.*
-- >             , packageY >= 1.0 && < 1.5
-- >             , packageZ != 0.1
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
    -- | Package name and version, drawn from the @name@ and @version@ keys.
    package      :: PackageID
    -- | Package software license type, drawn from the @license@ key.
  , license      :: License
    -- | Package software license text, drawn from the @license-file@ key.
  , licenseFile  :: FilePath
    -- | Package copyright holder, drawn from the @copyright@ key.
  , copyright    :: T.Text
    -- | Package maintainer email, drawn from the @maintainer@ key.
  , maintainer   :: T.Text
    -- | Package author name, drawn from the @author@ key.
  , author       :: T.Text
    -- | Package homepage URL, drawn from the @homepage@ key.
  , homepage     :: T.Text
    -- | Package bug report URL, drawn from the @bug-reports@ key.
  , bugReports   :: T.Text
    -- | One-line package summary, drawn from the @synopsis@ key.
  , synopsis     :: T.Text
    -- | Package dependencies as listed in the @pkg.fpm@ file, not to be
    --   confused with the package dependency closure computed by dependency
    --   resolution. This is drawn from the @dependencies@ key.
  , dependencies :: [Dependency]
    -- | Package specification version, drawn from the @fpm-version@ key.
  , specVersion  :: Version
    -- | Exposed modules, drawn from the module sections.
  , modules      :: NonEmpty Module
    -- | Language bindings, drawn from the binding sections.
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
