{-|
Module      : Flipper.Distribution.SymbolName
Description : Flipper Packages
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

-}

{-# LANGUAGE DeriveDataTypeable
           , DeriveGeneric
           , GeneralizedNewtypeDeriving
           #-}

module Flipper.Distribution.SymbolName (
    SymbolName()
  , parseSymbolName
  ) where

import Control.Applicative

import Control.DeepSeq

import Data.Binary

import Data.Data

import qualified Data.Text as T

import GHC.Generics

import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Text as M


-- | A module symbol name. This must be a legal C identifier. A legal C
--   identifier is any string recognized by the following grammer that is not
--   equal to a C reserved word.
--
-- > identifier = nondigit
-- >            | identifier digit
-- >            | identifier nondigit
-- >
-- > nondigit = '_' | 'a' | 'b' | 'B' | 'c' | 'C' | 'd' | 'D' | 'e' | 'E' | 'f'
-- >          | 'F' | 'g' | 'G' | 'h' | 'H' | 'i' | 'I' | 'j' | 'J' | 'k' | 'K'
-- >          | 'l' | 'L' | 'm' | 'M' | 'n' | 'N' | 'o' | 'O' | 'p' | 'P' | 'q'
-- >          | 'Q' | 'r' | 'R' | 's' | 'S' | 't' | 'T' | 'u' | 'U' | 'v' | 'V'
-- >          | 'w' | 'W' | 'x' | 'X' | 'y' | 'Y  | 'z' | 'Z'
-- >
-- > digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
--
--   C keywords:
--
-- - auto
-- - break
-- - case
-- - char
-- - const
-- - continue
-- - default
-- - do
-- - double
-- - else
-- - enum
-- - extern
-- - float
-- - for
-- - goto
-- - if
-- - int
-- - long
-- - register
-- - return
-- - short
-- - signed
-- - sizeof
-- - static
-- - struct
-- - switch
-- - typedef
-- - union
-- - unsigned
-- - void
-- - volatile
-- - while
newtype SymbolName = SymbolName { unSymbolName :: T.Text }
                   deriving ( Eq
                            , Ord
                            , Show
                            , Monoid
                            , Data
                            , Typeable
                            , NFData
                            , Binary
                            )

parseSymbolName :: M.Parser SymbolName
parseSymbolName = (SymbolName . T.pack) <$> (ident >>= notReserved)
    where ident = ((:) <$> nondigit) <*> toks
          nondigit =  M.letterChar <|> M.char '_'
          toks = M.many (M.letterChar <|> M.digitChar <|> M.char '_')
          notReserved n
                | n `elem` res = fail "parseSymbolName: identifier reserved."
                | otherwise    = pure n
          res = [ "auto"
                , "break"
                , "case"
                , "char"
                , "const"
                , "continue"
                , "default"
                , "do"
                , "double"
                , "else"
                , "enum"
                , "extern"
                , "float"
                , "for"
                , "goto"
                , "if"
                , "int"
                , "long"
                , "register"
                , "return"
                , "short"
                , "signed"
                , "sizeof"
                , "static"
                , "struct"
                , "switch"
                , "typedef"
                , "union"
                , "unsigned"
                , "void"
                , "volatile"
                , "while"
                ]
