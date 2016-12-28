{-|
Module      : Flipper.Distribution.Package
Description : Flipper Packages
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX
-}

{-# LANGUAGE GeneralizedNewtypeDeriving
           , TypeFamilies
           #-}

module Flipper.Distribution.Package where

import qualified Data.Text as T

import Flipper.Distribution.License
import Flipper.Distribution.Version

-- | A legal package name is something I still need to think more about...
newtype PackageName = PackageName { unPackageName :: T.Text }
                    deriving (Eq, Ord, Show)

-- | A name and version uniquely identifies a package.
data PackageID = PackageID {
    pkgName    :: PackageName
  , pkgVersion :: Version
  } deriving (Eq, Ord, Show)

-- | A name and version range identifies a package dependency.
data Dependency = Dependency {
    depName  :: PackageName
  , depRange :: VersionRange
  } deriving (Eq, Show)

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
    --   confused the package dependency closure computed by dependency
    --   resolution.
  , dependencies :: [Dependency]
    -- | Package specification version.
  , specVersion  :: Version
    -- | Exposed modules.
  , modules      :: [Module]
  } deriving (Eq, Show)

-- | A module.
data Module = Module
            deriving (Eq, Ord, Show)
