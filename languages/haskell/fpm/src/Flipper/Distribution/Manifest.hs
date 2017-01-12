{-|
Module      : Flipper.Distribution.Manifest
Description : Flipper Packages
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides the first-pass parser for @pkg.fpm@ files and a processing
monad for accessing the resulting keys and sections.
-}

{-# LANGUAGE BangPatterns
           , DeriveDataTypeable
           , DeriveGeneric
           , GeneralizedNewtypeDeriving
           , TupleSections
           #-}

module Flipper.Distribution.Manifest (
    Manifest(..)
  , parseManifest



  , ManifestError(..)
  , manifestErrorPretty
  , ManifestP()
  , runManifestP
  , liftParser
  , headerKey
  , optionalHeaderKey
  , procModuleSections
  , moduleKey
  , optionalModuleKey
  , procBindingSections
  , bindingKey
  , optionalBindingKey
  ) where

import Control.Applicative

import Control.DeepSeq

import Control.Monad
import Control.Monad.Fix

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.Binary

import Data.Data

import Data.Foldable

import Data.List.NonEmpty

import qualified Data.Map as M

import Data.Monoid

import qualified Data.Text as T

import Flipper.Distribution.Language
import Flipper.Distribution.Parser
import Flipper.Distribution.SymbolName

import GHC.Generics

import qualified Text.Megaparsec       as M
import qualified Text.Megaparsec.Error as M
import qualified Text.Megaparsec.Text  as M

type TextPairs = M.Map T.Text T.Text

-- | Internal representation of @pkg.fpm@ files after the first parsing pass.
data Manifest = Manifest {
    -- | Header key-value pairs before any module or binding sections.
    header       :: TextPairs
    -- | Module sections indexed by module names.
  , modSections  :: M.Map SymbolName TextPairs
    -- | Binding sections indexed by language.
  , bindSections :: M.Map Language TextPairs
  } deriving ( Eq
             , Ord
             , Show
             , Data
             , Typeable
             , Generic
             )

instance NFData Manifest
instance Binary Manifest

-- | First-pass parser. This parser provides the 'Manifest' type that parsers in
--   the 'ManifestP' monad use. Consumes all input.
parseManifest :: M.Parser Manifest
parseManifest = do
    hs       <- parsePairs
    (ms, bs) <- parseSections
    M.eof
    pure $ Manifest hs ms bs

-- | Consumes all pairs until a section declaration or end of input are
--   encountered.
parsePairs :: M.Parser TextPairs
parsePairs = go M.empty
    where go m = M.option m ((M.try kv) >>= ins m >>= go)
          kv   = do k <- lexed key
                    symb ":"
                    -- val will consume the newline:
                    v <- T.pack <$> val
                    pure (k, v)
          ins m (!k, !v) = case M.lookup k m of
                                   Nothing -> pure (M.insert k v m)
                                   _       -> fail $ mconcat [ "Duplicate key "
                                                             , T.unpack k
                                                             , "\n"
                                                             ]
          key = T.pack <$> M.some (M.choice [ M.alphaNumChar
                                            , M.char '-'
                                            ])
          val :: M.Parser String
          val = do c <- M.anyChar
                   case c of '\n' -> M.option [] (M.try indentCont)
                             x    -> (x :) <$> val
          indentCont = (M.char ' ' <|> M.char '\t')
                    *> spaceEater
                    *> ((' ' :) <$> val)

parseSections :: M.Parser (M.Map SymbolName TextPairs, M.Map Language TextPairs)
parseSections = M.many someSection >>= foldM appUpdate (M.empty, M.empty)
    where someSection = M.space *> M.eitherP (M.try parseModSection)
                                             (M.try parseBindSection)
          appUpdate (!ms, !bs) (Left mu)  = (,bs) <$> insms mu ms
          appUpdate (!ms, !bs) (Right bu) = (ms,) <$> insbs bu bs
          insms (!sn, !ps) ms =
              case M.lookup sn ms of
                      Nothing -> pure (M.insert sn ps ms)
                      _       -> fail $ mconcat [ "Duplicate module section "
                                                , symbolNamePretty sn
                                                , "\n"
                                                ]
          insbs (!ln, !ps) bs =
              case M.lookup ln bs of
                      Nothing -> pure (M.insert ln ps bs)
                      _       -> fail $ mconcat [ "Duplicate binding section "
                                                , languagePretty ln
                                                , "\n"
                                                ]

parseModSection :: M.Parser (SymbolName, TextPairs)
parseModSection = do
    M.string "module "
    sn <- parseSymbolName
    M.char '\n'
    ps <- parsePairs
    pure (sn, ps)

parseBindSection :: M.Parser (Language, TextPairs)
parseBindSection = do
    M.string "binding "
    ln <- M.try (parseLanguage <* M.char '\n')
          <|> Unknown <$> (word <* M.char '\n')
    ps <- parsePairs
    pure (ln, ps)

type ParseError = M.ParseError Char M.Dec

-- | A manifest processing error. This could be a parser error, or a missing or
--   superfluous key or section.
data ManifestError = ParserError ParseError
                   | NoHeaderKey T.Text
                   | ExtraHeaderKey T.Text
                   | NoModules
                   | NoModuleKey SymbolName T.Text
                   | ExtraModuleKey SymbolName T.Text
                   | NoBindingKey Language T.Text
                   | ExtraBindingKey Language T.Text

manifestErrorPretty :: ManifestError -> String
manifestErrorPretty (ParserError e) =
    mconcat [ "Parser error.\n"
           , M.parseErrorPretty e
           ]
manifestErrorPretty (NoHeaderKey k) =
    mconcat [ "Missing expected header key \""
            , T.unpack k
            , "\"\n"
            ]
manifestErrorPretty (ExtraHeaderKey k) =
    mconcat [ "Unexpected header key \""
            , T.unpack k
            , "\"\n"
            ]
manifestErrorPretty NoModules =
    "Manifest file must contain at least one module declaration.\n"
manifestErrorPretty (NoModuleKey m k) =
    mconcat [ "Missing expected key \""
            , T.unpack k
            , "\" from declaration for module \""
            , symbolNamePretty m
            , "\"\n"
            ]
manifestErrorPretty (ExtraModuleKey m k) =
    mconcat [ "Unexpected key  \""
            , T.unpack k
            , "\" in declaration for module \""
            , symbolNamePretty m
            , "\"\n"
            ]
manifestErrorPretty (NoBindingKey b k) =
    mconcat [ "Missing expected key \""
            , T.unpack k
            , "\" from declaration for binding \""
            , languagePretty b
            , "\"\n"
            ]
manifestErrorPretty (ExtraBindingKey b k) =
    mconcat [ "Unexpected key \""
            , T.unpack k
            , "\" in declaration for binding\""
            , "\"\n"
            ]

-- | Manifest processing monad. Provides 'ExceptT' for error reporting and
--   'StateT' for controlled access to the parsed 'Manifest'.
newtype ManifestP a = ManifestP {
    unManifestP ::  ReaderT String (ExceptT ManifestError (State Manifest)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFix
             )

runManifestP :: ManifestP a
             -> String -- ^ File name for error reporting.
             -> T.Text -- ^ Parser input.
             -> Either ManifestError a
runManifestP (ManifestP p) fn input = case M.runParser parseManifest fn input of
    Left e  -> Left (ParserError e)
    Right m -> evalState (runExceptT (runReaderT p fn)) m

fileName :: ManifestP String
fileName = ManifestP ask

throwME :: ManifestError -> ManifestP a
throwME = ManifestP . lift . throwE

-- | Zucc the key out of the map, if it exists, returning the value and updated
--   map.
zucc :: Ord k => k -> M.Map k v -> (Maybe v, M.Map k v)
zucc = M.updateLookupWithKey (\_ _ -> Nothing)

getME :: (Manifest -> a) -> ManifestP a
getME = ManifestP . lift . lift . gets

modME :: (Manifest -> Manifest) -> ManifestP ()
modME = ManifestP . lift . lift . modify'

liftParser :: M.Parser a -> T.Text -> ManifestP a
liftParser p t = do
    fn <- fileName
    case M.runParser (rhs p) fn t of Right r -> pure r
                                     Left e  -> throwME (ParserError e)

headerKey :: T.Text -> ManifestP T.Text
headerKey k = do
    (v, m') <- zucc k <$> getME header
    case v of Nothing  -> throwME (NoHeaderKey k)
              (Just r) -> do modME (\s -> s { header = m' })
                             pure r

optionalHeaderKey :: T.Text -> ManifestP (Maybe T.Text)
optionalHeaderKey k = do
    (v, m') <- zucc k <$> getME header
    modME (\s -> s { header = m' })
    pure v

procModuleSections :: (SymbolName -> ManifestP a)
                   -> ManifestP (NonEmpty a)
procModuleSections f = do
    ks <- (nonEmpty . M.keys) <$> getME modSections
    case ks of Nothing  -> throwME NoModules
               (Just l) -> traverse f l

moduleKey :: SymbolName -> T.Text -> ManifestP T.Text
moduleKey m k = do
    sm <- getME modSections
    case M.lookup m sm of
        Nothing -> error "moduleKey: absurd! This is a serious bug in FPM! \
                         \Please report this."
        Just mm -> let (v, mm') = zucc k mm
                       sm' = M.insert m mm' sm
                   in case v of
                       Nothing -> throwME (NoModuleKey m k)
                       Just r  -> do modME (\s -> s { modSections = sm' })
                                     pure r

optionalModuleKey :: SymbolName -> T.Text -> ManifestP (Maybe T.Text)
optionalModuleKey m k = do
    sm <- getME modSections
    case M.lookup m sm of
        Nothing -> error "optModuleKey: absurd! This is a serious bug in FPM! \
                         \Please report this."
        Just mm -> let (v, mm') = zucc k mm
                       sm' = M.insert m mm' sm
                   in do modME (\s -> s { modSections = sm' })
                         pure v

procBindingSections :: (Language -> ManifestP a)
                    -> ManifestP [a]
procBindingSections f = (M.keys <$> getME bindSections) >>= traverse f

bindingKey :: Language -> T.Text -> ManifestP T.Text
bindingKey b k = do
    bm <- getME bindSections
    case M.lookup b bm of
        Nothing -> error "bindingKey: absurd! This is a serious bug in FPM! \
                         \Please report this."
        Just mm -> let (v, mm') = zucc k mm
                       bm' = M.insert b mm' bm
                   in case v of
                       Nothing -> throwME (NoBindingKey b k)
                       Just r  -> do modME (\s -> s { bindSections = bm' })
                                     pure r

optionalBindingKey :: Language -> T.Text -> ManifestP (Maybe T.Text)
optionalBindingKey b k = do
    bm <- getME bindSections
    case M.lookup b bm of
        Nothing -> error "optBindingKey: absurd! This is a serious bug in FPM! \
                         \Please report this."
        Just mm -> let (v, mm') = zucc k mm
                       bm' = M.insert b mm' bm
                   in do modME (\s -> s { bindSections = bm' })
                         pure v
