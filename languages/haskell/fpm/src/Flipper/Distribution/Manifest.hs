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

{-# LANGUAGE DeriveDataTypeable
           , DeriveGeneric
           , GeneralizedNewtypeDeriving
           #-}

module Flipper.Distribution.Manifest where

import Control.DeepSeq

import Control.Monad.Fail
import Control.Monad.Fix

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.Binary

import Data.Data

import Data.List.NonEmpty

import qualified Data.Map as M

import Data.Monoid

import qualified Data.Text as T

import Flipper.Distribution.Language
import Flipper.Distribution.SymbolName

import GHC.Generics

import qualified Text.Megaparsec       as M
import qualified Text.Megaparsec.Error as M
import qualified Text.Megaparsec.Text  as M

-- | Internal representation of @pkg.fpm@ files after the first parsing pass.
data Manifest = Manifest {
    -- | Header key-value pairs before any module or binding sections.
    header       :: M.Map T.Text T.Text
    -- | Module sections indexed by module names.
  , modSections  :: M.Map SymbolName (M.Map T.Text T.Text)
    -- | Binding sections indexed by language.
  , bindSections :: M.Map Language (M.Map T.Text T.Text)
  } deriving ( Eq
             , Ord
             , Show
             , Data
             , Typeable
             , Generic
             )

instance NFData Manifest
instance Binary Manifest

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
manifestErrorPretty (ParserError e)     = "Parser error.\n"
                                       <> M.parseErrorPretty e
manifestErrorPretty (NoHeaderKey k)     = "Missing expected header key \""
                                       <> T.unpack k
                                       <> "\"\n"
manifestErrorPretty (ExtraHeaderKey k) = "Unexpected header key \""
                                      <> T.unpack k
                                      <> "\"\n"
manifestErrorPretty NoModules             = "Manifest file must contain at \
                                            \ least one module declaration.\n"
manifestErrorPretty (NoModuleKey m k)     = "Missing expected key \""
                                         <> T.unpack k
                                         <> "\" from declaration for module \""
                                         <> symbolNamePretty m
                                         <> "\"\n"
manifestErrorPretty (ExtraModuleKey m k)  = "Unexpected key  \""
                                         <> T.unpack k
                                         <> "\" in declaration for module \""
                                         <> symbolNamePretty m
                                         <> "\"\n"
manifestErrorPretty (NoBindingKey b k)    = "Missing expected key \""
                                         <> T.unpack k
                                         <> "\" from declaration for binding \""
                                         <> languagePretty b
                                         <> "\"\n"
manifestErrorPretty (ExtraBindingKey b k) = "Unexpected key \""
                                         <> T.unpack k
                                         <> "\" in declaration for binding\""
                                         <> "\"\n"

-- | Manifest processing monad. Provides 'ExceptT' for error reporting and
--   'StateT' for controlled access to the parsed 'Manifest'.
newtype ManifestP a = ManifestP {
    unManifestP ::  ReaderT String (ExceptT ManifestError (State Manifest)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFix
             )

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
    case M.runParser p fn t of Right r -> pure r
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

moduleSections :: ManifestP (NonEmpty SymbolName)
moduleSections = do
    ks <- (nonEmpty . M.keys) <$> getME modSections
    case ks of Nothing  -> throwME NoModules
               (Just l) -> pure l

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

bindingSections :: ManifestP [Language]
bindingSections = M.keys <$> getME bindSections

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
