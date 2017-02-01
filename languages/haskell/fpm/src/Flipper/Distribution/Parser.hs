{-|
Module      : Flipper.Distribution.Parser
Description : Flipper Package Management
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

-- | Consume any and all whitespace (except for newlines, which have special behavior in
--   manifest files) and comments.
spaceEater :: M.Parser ()
spaceEater = ML.space (M.skipSome (MC.char ' ' <|> MC.tab))
                      (ML.skipLineComment "--")
                      (ML.skipBlockCommentNested "{-" "-}")

-- | Parse any lexical element.
lexed :: M.Parser a -> M.Parser a
lexed = ML.lexeme spaceEater

-- | Parse a lexical symbol.
symb :: String -> M.Parser String
symb = ML.symbol spaceEater

-- | Force a parser to consume all of its input.
greedy :: M.Parser a -> M.Parser a
greedy = (<* M.eof)

-- | Make a parser lexical and greedy.
rhs :: M.Parser a -> M.Parser a
rhs = greedy . lexed

-- | Consumes any visible Unicode character.
visible :: M.Parser Char
visible = M.choice [ MC.alphaNumChar
                   , MC.numberChar
                   , MC.punctuationChar
                   , MC.symbolChar
                   ]

-- | Consomes a contiguous non-empty sequence of visible Unicode characters.
word :: M.Parser T.Text
word = T.pack <$> some visible
