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

import qualified Data.Text as T

import qualified Text.Megaparsec            as M
import qualified Text.Megaparsec.Char       as MC
import qualified Text.Megaparsec.Combinator as M
import qualified Text.Megaparsec.Lexer      as ML
import qualified Text.Megaparsec.Text       as M

spaceEater :: M.Parser ()
spaceEater = ML.space (M.skipSome (MC.char ' ' <|> MC.tab))
                      (ML.skipLineComment "--")
                      (ML.skipBlockCommentNested "{-" "-}")

lexed :: M.Parser a -> M.Parser a
lexed = ML.lexeme spaceEater

symb :: String -> M.Parser String
symb = ML.symbol spaceEater

greedy :: M.Parser a -> M.Parser a
greedy = (<* M.eof)

rhs :: M.Parser a -> M.Parser a
rhs = greedy . lexed

visible :: M.Parser Char
visible = M.choice [ MC.alphaNumChar
                   , MC.numberChar
                   , MC.punctuationChar
                   , MC.symbolChar
                   ]

word :: M.Parser T.Text
word = T.pack <$> some visible
