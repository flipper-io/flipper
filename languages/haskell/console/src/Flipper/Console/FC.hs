{-|
Module      : Flipper.Console.FC
Description : Flipper Console Monad
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides the 'FC' monad.
-}

module Flipper.Console.FC (
    FC
  , liftFC
  , runFC
  , printFC
  , printCStringFC
  , reportConsoleError
  ) where

import Control.Monad.Trans.Class

import Flipper.Error
import Flipper.MonadFlipper

import Flipper.Console.Action
import Flipper.Console.Error

import System.Console.Haskeline

-- | The Flipper Console monad, providing:
--   - 'FlipperT' for interacting with the device.
--   - 'InputT' for command line interaction.
--   - 'IO'.
type FC = FlipperT (InputT IO)

-- | Lift an 'IO' action into the 'FC' monad.
liftFC :: IO a -> FC a
liftFC = lift . lift

-- | Run an 'FC' action in 'IO', capturing any 'FlipperException's that are
--   thrown.
runFC :: FC a -> IO (Either FlipperException a)
runFC = runInputT defaultSettings . runFlipperT

-- | Show a value on the command line.
printFC :: Show a => a -> FC ()
printFC = lift . outputStrLn . show

-- | Show a 'CString' (or encoding error) on the command line.
printCStringFC :: Either String String -> FC ()
printCStringFC (Left e)  = lift $ outputStrLn e
printCStringFC (Right v) = lift $ outputStrLn v

-- | Report a 'FlipperException' caught during execution of a 'ConsoleAction'.
reportConsoleError :: Maybe ConsoleAction -> FlipperException -> FC ()
reportConsoleError c (FlipperException e) =
    lift (outputStrLn (consoleError c e))
