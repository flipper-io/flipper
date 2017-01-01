{-|
Module      : Flipper.Distribution.Parser
Description : Flipper Packages
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides shared parser code.
-}

module Flipper.Distribution.Parser where

import Control.Applicative

import qualified Data.Map as M

import qualified Data.Text as T

import qualified Text.Megaparsec.Char       as MC
import qualified Text.Megaparsec.Combinator as M
import qualified Text.Megaparsec.Lexer      as M
import qualified Text.Megaparsec.Text       as M

spaceEater :: M.Parser ()
spaceEater = M.space (M.skipSome (MC.char ' ' <|> MC.tab))
                     (M.skipLineComment "--")
                     (M.skipBlockCommentNested "{-" "-}")

lexed :: M.Parser a -> M.Parser a
lexed = M.lexeme spaceEater

symb :: String -> M.Parser String
symb = M.symbol spaceEater

visible :: M.Parser Char
visible = M.choice [ MC.alphaNumChar
                   , MC.numberChar
                   , MC.punctuationChar
                   , MC.symbolChar
                   ]

word :: M.Parser T.Text
word = T.pack <$> some visible
