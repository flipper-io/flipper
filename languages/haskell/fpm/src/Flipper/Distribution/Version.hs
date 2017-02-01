{-|
Module      : Flipper.Distribution.Version
Description : Flipper Package Management
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module defines package version numbers and version range predicates.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving
           , DeriveDataTypeable
           , DeriveGeneric
           , FlexibleInstances
           , TypeFamilies
           #-}

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
  , parseVersion
  , parseVersionRange
  ) where

import Control.Applicative

import Control.DeepSeq

import Data.Binary

import Data.Data

import Data.List.NonEmpty as NE

import Data.Maybe

import Flipper.Distribution.Parser

import GHC.Exts
import GHC.Generics

import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Combinator as M
import qualified Text.Megaparsec.Expr       as M
import qualified Text.Megaparsec.Lexer      as M
import qualified Text.Megaparsec.Text       as M

-- | A legal package version is any sequence of one or more positive integers
--   separated by periods. Versions are compared lexicographically, i.e.
--   3.0 > 2.9, 2.1 > 2.0, 1.2.3 > 1.2.2, etc.
newtype Version = Version { unVersion :: NonEmpty Integer }
                deriving ( Eq
                         , Ord
                         , Show
                         , Data
                         , Typeable
                         , Generic
                         , NFData
                         )

-- TODO: Remove this when the Stackage LTS moves to binary version 0.8.4.0 >=
instance Binary (NonEmpty Integer)
instance Binary Version

-- | Returns 'Nothing' if any of the 'Int's are less than zero.
mkVersion :: Integral a => [a] -> Maybe Version
mkVersion is = Version <$> (nonEmpty is >>= mapM (notNeg . toInteger))
    where notNeg n
            | n < 0     = Nothing
            | otherwise = Just n

-- | Careful, this lazily checks whether or not the list literal's integers are
--   non-negative.
instance IsList Version where
    type Item Version = Integer
    fromList = fromMaybe (error e) . mkVersion
        where e = "fromList: bad Version"
    toList = NE.toList . unVersion

-- | A 'Version' range predicate. Beware this type's 'Eq' instance; ranges are
--   not canonicalized, so 'VersionRange's describing identical intervals won't
--   necessary be equal according to '(==)'. Constructors aren't exported
--   because we might change to an interval-based representation later (which
--   would solve the canonicalization problem and make it possible to warn the
--   user about unsatisfiable predicates).
data VersionRange = Any
                  | This Version
                  | Later Version
                  | Earlier Version
                  | Union VersionRange VersionRange
                  | Intersection VersionRange VersionRange
                  deriving ( Eq
                           , Ord
                           , Show
                           , Data
                           , Typeable
                           , Generic
                           )

instance NFData VersionRange
instance Binary VersionRange

-- | Any version.
anyVersion :: VersionRange
anyVersion = Any

-- | Unsatisfiable version range.
noVersion :: VersionRange
noVersion = Intersection (Later v) (Earlier v)
    where v = Version (1 :| [])

-- | Version range @== v@.
thisVersion :: Version -> VersionRange
thisVersion = This

-- | Version range @!= v@.
notThisVersion :: Version -> VersionRange
notThisVersion v = Intersection (Earlier v) (Later v)

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
--
-- > forall r v. inRange r v == not (inRange (inverse r) v)
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
    where incLS (p :| [])      = (p + 1) :| []
          incLS (p :| (p':ps)) = p <| incLS (p' :| ps)

-- | Check whether or not a 'Version' is in a 'VersionRange'
inRange :: VersionRange -> Version -> Bool
inRange Any                = const True
inRange (This v)           = (v ==)
inRange (Later v)          = (> v)
inRange (Earlier v)        = (< v)
inRange (Union a b)        = (\v -> inRange a v || inRange b v)
inRange (Intersection a b) = (\v -> inRange a v && inRange b v)

parseVersion :: M.Parser Version
parseVersion = (Version . NE.fromList) -- sepBy1 will return non-empty list.
           <$> M.sepBy1 M.decimal (M.char '.')

parseWildVersion :: M.Parser Version
parseWildVersion = (Version . NE.fromList) -- endBy1 will return non-empty list.
               <$> (M.endBy1 M.decimal (M.char '.') <* M.char '*')

parseVersionRange :: M.Parser VersionRange
parseVersionRange = expr
    where expr = M.makeExprParser term table
          term = M.choice [ parens expr
                          , symb "==" *> M.choice [ M.try (thisVersion <$> vers)
                                                  , within <$> wild
                                                  ]
                          , symb "!=" *> (notThisVersion <$> vers)
                          , symb ">=" *> (orLaterVersion <$> vers)
                          , symb ">" *> (laterVersion <$> vers)
                          , symb "<=" *> (orEarlierVersion <$> vers)
                          , symb "<" *> (earlierVersion <$> vers)
                          ]
          table = [ [M.InfixL (symb "&&" *> pure intersection)]
                  , [M.InfixL (symb "||" *> pure union)]
                  ]
          parens = M.between (symb "(") (symb ")")
          vers = lexed parseVersion
          wild = lexed parseWildVersion
