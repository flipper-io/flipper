{-|
Module      : Flipper.Distribution.Version
Description : Flipper Packages
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module defines package version numbers and version range predicates.
-}

{-# LANGUAGE TypeFamilies #-}

module Flipper.Distribution.Version (
    Version()
  , VersionRange()
  , anyVersion
  , noVersion
  , thisVersion
  , notThisVersion
  , laterVersion
  , earlierVersion
  , orLaterVersion
  , orEarlierVersion
  , union
  , intersection
  , inverse
  , within
  , inRange
  ) where

import Data.Maybe

import GHC.Exts

-- | A legal package version is any sequence of one or more positive integers
--   separated by periods. Versions are compared lexicographically, i.e.
--   3.0 > 2.9, 2.1 > 2.0, 1.2.3 > 1.2.2, etc.
newtype Version = Version { unVersion :: [Int] }
                deriving (Eq, Ord, Show)

mkVersion :: [Int] -> Maybe Version
mkVersion = fmap Version . mapM notNeg
    where notNeg n
            | n < 0     = Nothing
            | otherwise = Just n

-- | Careful, this lazily checks whether or not the list literal's integers are
--   non-negative.
instance IsList Version where
    type Item Version = Int
    fromList = fromMaybe (error e) . mkVersion
        where e = "fromList: version numbers must be >= 0."
    toList = unVersion

-- | A 'Version' range predicate.
data VersionRange = Any
                  | This Version
                  | Later Version
                  | Earlier Version
                  | Union VersionRange VersionRange
                  | Intersection VersionRange VersionRange
                  deriving (Eq, Show)

-- | Any version.
anyVersion :: VersionRange
anyVersion = Any

-- | Unsatisfiable version range.
noVersion :: VersionRange
noVersion = Intersection (Later v) (Earlier v)
    where v = Version [1]

-- | Version range @== v@.
thisVersion :: Version -> VersionRange
thisVersion = This

-- | Version range @!= v@.
notThisVersion :: Version -> VersionRange
notThisVersion v = Union (Earlier v) (Later v)

-- | Version range @> v@.
laterVersion :: Version -> VersionRange
laterVersion = Later

-- | Version range @< v@.
earlierVersion :: Version -> VersionRange
earlierVersion = Earlier

-- | Version range @>= v@
orLaterVersion :: Version -> VersionRange
orLaterVersion v = Union (This v) (Later v)

-- | Version range @<= v@
orEarlierVersion :: Version -> VersionRange
orEarlierVersion v = Union (This v) (Earlier v)

-- | Range disjunction.
union :: VersionRange -> VersionRange -> VersionRange
union = Union

-- | Range conjunction.
intersection :: VersionRange -> VersionRange -> VersionRange
intersection = Intersection

-- | Invert a range.
-- > inRange r v == not (inRange (inverse r) v)
inverse :: VersionRange -> VersionRange
inverse Any                = noVersion
inverse (This v)           = notThisVersion v
inverse (Later v)          = orEarlierVersion v
inverse (Earlier v)        = orLaterVersion v
inverse (Union a b)        = Intersection (inverse a) (inverse b)
inverse (Intersection a b) = Union (inverse a) (inverse b)

-- | Version range @== v.*@
within :: Version -> VersionRange
within v = Intersection (orLaterVersion v)
                        (Earlier (Version (incLS (unVersion v))))
    where incLS []     = error "within: empty version!"
          incLS [p]    = [p + 1]
          incLS (p:ps) = p : incLS ps

-- | Check whether or not a 'Version' is in a 'VersionRange'
inRange :: VersionRange -> Version -> Bool
inRange Any                = const True
inRange (This v)           = (v ==)
inRange (Later v)          = (> v)
inRange (Earlier v)        = (< v)
inRange (Union a b)        = (\v -> inRange a v || inRange b v)
inRange (Intersection a b) = (\v -> inRange a v && inRange b v)
