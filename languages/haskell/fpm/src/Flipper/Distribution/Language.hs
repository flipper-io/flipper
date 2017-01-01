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

module Flipper.Distribution.Language (
    Language(..)
  , parseLanguage
  , languagePretty
  , LangSrc(..)
  , parseLangSrc
  ) where

import Control.DeepSeq

import Data.Binary

import Data.Data

import Data.Monoid

import qualified Data.Text as T

import Flipper.Distribution.Parser

import GHC.Generics

import Text.Megaparsec      as M
import Text.Megaparsec.Text as M

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
                -- | Research how F# people do things.
              | FS
                -- | FPM can install Haskell bindings from Hackage.
              | Haskell
                -- | Research how Java people do things.
              | Java
                -- | FPM can install NodeJS bindings from NPM.
              | NodeJS
                -- | Research how ObjC people do things.
              | ObjC
                -- | FPM can install OCaml bindings from OPAM.
              | OCaml
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
                       , Show
                       , Data
                       , Typeable
                       , Generic
                       , NFData
                       , Binary
                       )

-- | 'Language' parser. This parser is lazy and doesn't know how the language
--   string should be terminated, so it can't parse an 'Unknown' language. To do
--   so, simply combine this parser with an alternative that accepts any
--   'M.word' in the failure case. For example, here's a 'Language' parser that
--   consumes the entire input (i.e. delimited by EOF):
--
-- > knownOrUknown = try (parseLanguage <* eof) <|> Unknown <$> (word <* eof)
--
-- >>> parseTest knownOrUnknown "python"
-- Python
-- >>> parseTest knownOrUnknown "APL"
-- Unknwon "APL"
parseLanguage :: Parser Language
parseLanguage = choice [ string' "commonlisp" *> pure CommonLisp
                       , string' "cl" *> pure CommonLisp
                       , string' "common-lisp" *> pure CommonLisp
                       , string' "common_lisp" *> pure CommonLisp
                       , string' "common lisp" *> pure CommonLisp
                       , string' "c++" *> pure CPP
                       , string' "cpp" *> pure CPP
                       , string' "cplusplus" *> pure CPP
                       , string' "c#" *> pure CS
                       , string' "cs" *> pure CS
                       , string' "csharp" *> pure CS
                       , string' "c-sharp" *> pure CS
                       , string' "c_sharp" *> pure CS
                       , string' "cosmonaut" *> pure Rust
                       , string' "c" *> pure C
                       , string' "microsoft java" *> pure CS
                       , string' "d" *> pure D
                       , string' "f#" *> pure FS
                       , string' "fs" *> pure FS
                       , string' "fsharp" *> pure CS
                       , string' "f-sharp" *> pure CS
                       , string' "f_sharp" *> pure CS
                       , string' "microsoft ml" *> pure CS
                       , string' "haskell" *> pure Haskell
                       , string' "astronaut" *> pure Haskell
                       , string' "java" *> pure Java
                       , string' "js" *> pure NodeJS
                       , string' "nodejs" *> pure NodeJS
                       , string' "node" *> pure NodeJS
                       , string' "objectivec" *> pure ObjC
                       , string' "objective-c" *> pure ObjC
                       , string' "objective_c" *> pure ObjC
                       , string' "obj-c" *> pure ObjC
                       , string' "obj_c" *> pure ObjC
                       , string' "apple java" *> pure ObjC
                       , string' "ocaml" *> pure OCaml
                       , string' "perl" *> pure Perl
                       , string' "moonspeak" *> pure Perl
                       , string' "python" *> pure Python
                       , string' "ruby" *> pure Ruby
                       , string' "rust" *> pure Rust
                       , string' "r" *> pure R
                       , string' "scala" *> pure Rust
                       , string' "scheme" *> pure Scheme
                       , string' "swift" *> pure Swift
                       ]

languagePretty :: Language -> String
languagePretty C           = "C"
languagePretty CommonLisp  = "Common Lisp"
languagePretty CPP         = "C++"
languagePretty CS          = "C#"
languagePretty D           = "D"
languagePretty FS          = "F#"
languagePretty Haskell     = "Haskell"
languagePretty Java        = "Java"
languagePretty NodeJS      = "NodejS"
languagePretty ObjC        = "Objective C"
languagePretty OCaml       = "OCaml"
languagePretty Perl        = "Perl"
languagePretty Python      = "Python"
languagePretty R           = "R"
languagePretty Ruby        = "Ruby"
languagePretty Rust        = "Rust"
languagePretty Scala       = "Scala"
languagePretty Scheme      = "Scheme"
languagePretty Swift       = "Swift"
languagePretty (Unknown l) = (T.unpack l) <> " (unsupported)"

-- | A binding source. FPM knows how to install (or at least fetch) from these
--   sources on the user's behalf.
data LangSrc = -- | A Haskell package on Hackage.
               Hackage T.Text
               -- | An OCaml package on OPAM.
             | OPAM T.Text
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
                      , Show
                      , Data
                      , Typeable
                      , Generic
                      , NFData
                      , Binary
                      )

-- | TODO: Do some sanity checks here.
parseLangSrc :: Language -> Parser LangSrc
parseLangSrc C           = Git <$> word
parseLangSrc CommonLisp  = Git <$> word
parseLangSrc CPP         = Git <$> word
parseLangSrc CS          = Git <$> word
parseLangSrc D           = Git <$> word
parseLangSrc FS          = Git <$> word
parseLangSrc Haskell     = Hackage <$> word
parseLangSrc Java        = Git <$> word
parseLangSrc NodeJS      = NPM <$> word
parseLangSrc ObjC        = Git <$> word
parseLangSrc OCaml       = OPAM <$> word
parseLangSrc Perl        = CPAN <$> word
parseLangSrc Python      = PyPI <$> word
parseLangSrc R           = CRAN <$> word
parseLangSrc Ruby        = Git <$> word
parseLangSrc Rust        = Cargo <$> word
parseLangSrc Scala       = Git <$> word
parseLangSrc Scheme      = Git <$> word
parseLangSrc Swift       = Git <$> word
parseLangSrc (Unknown _) = Git <$> word
