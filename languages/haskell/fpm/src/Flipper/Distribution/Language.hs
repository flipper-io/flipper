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
  , LangSrc(..)
  , parseLangSrc
  ) where

import Control.DeepSeq

import Data.Binary

import Data.Data

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

-- | 'Language' parser
parseLanguage :: Parser Language
parseLanguage = choice [ lexed (string' "c" *> pure C)
                       , lexed (string' "commonlisp" *> pure CommonLisp)
                       , lexed (string' "cl" *> pure CommonLisp)
                       , lexed (string' "common-lisp" *> pure CommonLisp)
                       , lexed (string' "common_lisp" *> pure CommonLisp)
                       , lexed (string' "common lisp" *> pure CommonLisp)
                       , lexed (string' "c++" *> pure CPP)
                       , lexed (string' "cpp" *> pure CPP)
                       , lexed (string' "cplusplus" *> pure CPP)
                       , lexed (string' "c#" *> pure CS)
                       , lexed (string' "cs" *> pure CS)
                       , lexed (string' "csharp" *> pure CS)
                       , lexed (string' "c-sharp" *> pure CS)
                       , lexed (string' "c_sharp" *> pure CS)
                       , lexed (string' "microsoft java" *> pure CS)
                       , lexed (string' "d" *> pure D)
                       , lexed (string' "f#" *> pure FS)
                       , lexed (string' "fs" *> pure FS)
                       , lexed (string' "fsharp" *> pure CS)
                       , lexed (string' "f-sharp" *> pure CS)
                       , lexed (string' "f_sharp" *> pure CS)
                       , lexed (string' "microsoft ml" *> pure CS)
                       , lexed (string' "haskell" *> pure Haskell)
                       , lexed (string' "astronaut" *> pure Haskell)
                       , lexed (string' "java" *> pure Java)
                       , lexed (string' "js" *> pure NodeJS)
                       , lexed (string' "node" *> pure NodeJS)
                       , lexed (string' "nodejs" *> pure NodeJS)
                       , lexed (string' "objectivec" *> pure ObjC)
                       , lexed (string' "objective-c" *> pure ObjC)
                       , lexed (string' "objective_c" *> pure ObjC)
                       , lexed (string' "obj-c" *> pure ObjC)
                       , lexed (string' "obj_c" *> pure ObjC)
                       , lexed (string' "apple java" *> pure ObjC)
                       , lexed (string' "ocaml" *> pure OCaml)
                       , lexed (string' "perl" *> pure Perl)
                       , lexed (string' "moonspeak" *> pure Perl)
                       , lexed (string' "python" *> pure Python)
                       , lexed (string' "r" *> pure R)
                       , lexed (string' "ruby" *> pure Ruby)
                       , lexed (string' "rust" *> pure Rust)
                       , lexed (string' "cosmonaut" *> pure Rust)
                       , lexed (string' "scala" *> pure Rust)
                       , lexed (string' "scheme" *> pure Scheme)
                       , lexed (string' "swift" *> pure Swift)
                       , lexed (Unknown <$> word)
                       ]

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
parseLangSrc C           = Git <$> lexed word
parseLangSrc CommonLisp  = Git <$> lexed word
parseLangSrc CPP         = Git <$> lexed word
parseLangSrc CS          = Git <$> lexed word
parseLangSrc D           = Git <$> lexed word
parseLangSrc FS          = Git <$> lexed word
parseLangSrc Haskell     = Hackage <$> lexed word
parseLangSrc Java        = Git <$> lexed word
parseLangSrc NodeJS      = NPM <$> lexed word
parseLangSrc ObjC        = Git <$> lexed word
parseLangSrc OCaml       = OPAM <$> lexed word
parseLangSrc Perl        = CPAN <$> lexed word
parseLangSrc Python      = PyPI <$> lexed word
parseLangSrc R           = CRAN <$> lexed word
parseLangSrc Ruby        = Git <$> lexed word
parseLangSrc Rust        = Cargo <$> lexed word
parseLangSrc Scala       = Git <$> lexed word
parseLangSrc Scheme      = Git <$> lexed word
parseLangSrc Swift       = Git <$> lexed word
parseLangSrc (Unknown _) = Git <$> lexed word
