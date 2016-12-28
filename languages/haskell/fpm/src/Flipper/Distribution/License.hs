{-|
Module      : Flipper.Distribution.License
Description : Flipper Package Licenses
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides a data type for representing software licenses. Any license
may be used with Flipper packages, but FPM provides the full text for some of
the most common licenses for convenience.

= Disclaimer

The descriptions of software licenses provided by this documentation are
intended for informational purposes only and in no way constitute legal advice.
Please read the text of the licenses and consult a lawyer for any advice
regarding software licensing.
-}

{-# LANGUAGE DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           #-}

module Flipper.Distribution.License (
    License(..)
  , licenseText
  ) where

import Control.DeepSeq

import Data.Binary

import Data.Data

import qualified Data.Text as T

import GHC.Generics

-- | Licenses for source code release.
data License =
    -- | GNU General Public License,
    --   <https://www.gnu.org/licenses/old-licenses/gpl-2.0.html version 2>
    GPL2
    -- | GNU General Public License,
    --   <https://www.gnu.org/licenses/gpl.html version 3>
  | GPL3
    -- | GNU Affero General Public License,
    --   <https://www.gnu.org/licenses/agpl.html version 3>
  | AGPL
    -- | GNU Lesser General Public License,
    --   <https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html version 2.1>
  | LGPL21
    -- | GNU Lesser General Public License,
    --   <https://www.gnu.org/licenses/lgpl.html version 3>
  | LGPL3
    -- | BSD License,
    --   <http://www.opensource.org/licenses/bsd-license 2-clause>
  | BSD2
    -- | BSD License,
    --   <http://www.opensource.org/licenses/bsd-3-clause 3-clause>
  | BSD3
    -- | BSD License,
    --   <http://directory.fsf.org/wiki/License:BSD_4Clause 4-clause>
  | BSD4
    -- | <http://www.opensource.org/licenses/MIT MIT License>
  | MIT
    -- | Mozilla Public License,
    --   <https://www.mozilla.org/MPL/ version 2.0>
  | MPL2
    -- | Apache License,
    --   <https://www.apache.org/licenses/ version 2.0>
  | Apache2
    -- | The author of a package disclaims any copyright to its source code and
    --   dedicates it to the public domain. This is not a software license.
    --   Please note that it is not possible to dedicate works to the public
    --   domain in every jurisdiction, nor is a work that is in the public
    --   domain in one jurisdiction necessarily in the public domain elsewhere.
  | PublicDomain
    -- | Explicitly 'All Rights Reserved', eg for proprietary software. The
    --   package may not be legally modified or redistributed by anyone but the
    --   rightsholder.
  | AllRightsReserved
    -- | Any other software license.
  | Other
  deriving ( Eq
           , Ord
           , Read
           , Show
           , Enum
           , Data
           , Typeable
           , Generic
           , NFData
           , Binary
           )

-- | A license's body text, for automatically generating a LICENSE file.
licenseText :: License -> T.Text
licenseText = error "Implement me!"
