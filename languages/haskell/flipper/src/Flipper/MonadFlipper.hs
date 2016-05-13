{-|
Module      : Flipper.MonadFlipper
Description : The Flipper Monad
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

The Flipper monad and its typeclass.
-}

{-# LANGUAGE BangPatterns
           , TypeSynonymInstances
           , FlexibleInstances
           , LambdaCase
           #-}

module Flipper.MonadFlipper (
    MonadFlipper(..)
  , FlipperT(..)
  , Flipper(..)
  , mkBracket
  , bracketFlipper
  , bracketIO
  ) where

import Control.Exception

import Control.Monad.Except
import Control.Monad.IO.Class

import Flipper.Error
import Flipper.Internal.Error

-- | The class of monads in which Flipper actions may take place. A Flipper
--   monad must be able to perform IO and throw/catch 'FlipperException's.
class MonadIO m => MonadFlipper m where
    throwFlipper :: FlipperException -> m a
    catchFlipper :: m a -> (FlipperException -> m a) -> m a

instance MonadFlipper IO where
    throwFlipper = throwIO
    catchFlipper = catch

-- | The Flipper monad transformer.
type FlipperT = ExceptT FlipperException

-- | The Flipper monad.
type Flipper = FlipperT IO

instance MonadIO m => MonadFlipper (FlipperT m) where
    throwFlipper = throwError
    catchFlipper = catchError

-- | Clear any error condition, execute a Flipper action, and raise any
--   resulting error.
mkBracket :: MonadFlipper m
          => m FlipperError         -- ^ Error polling operation.
          -> m ()                   -- ^ Error clearing operation.
          -> (FlipperError -> m ()) -- ^ Error raising operation.
          -> m a                    -- ^ Lifted operation.
          -> m a
mkBracket g c r m = do
    c
    x <- m
    e <- g
    case e of OK -> return x
              _  -> r e >> throwFlipper (FlipperException e)

-- | Provides bracketing behavior with the default libflipper-provided error
--   handling functions.
bracketFlipper :: MonadFlipper m => m a -> m a
bracketFlipper = mkBracket (liftIO get) (liftIO clear) (liftIO . raise)

-- | Combined bracketing and lifting.
bracketIO :: MonadFlipper m => IO a -> m a
bracketIO = bracketFlipper . liftIO
