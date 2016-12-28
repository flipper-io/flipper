{-|
Module      : Main
Description : Flipper Console
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX
-}

module Main (
    main
  ) where

import Control.Monad

import Control.Monad.Trans.Class

import Flipper
import Flipper.MonadFlipper

import Flipper.Console.Exec
import Flipper.Console.FC
import Flipper.Console.Options

import Options.Applicative

import System.Console.Haskeline

-- | Act on the options obtained during start up. If the user provided a
--   'ConsoleAction' on the command line, run that. Otherwise, start the REPL.
runOptions :: Options -> IO ()
runOptions (Options e o) = (void . runFC) $ do
    s <- catchFlipper (attach e) ((pure False <*) . reportConsoleError Nothing)
    if s then (case o of
                   Nothing  -> fcREPL
                   (Just c) -> catchFlipper (execConsoleAction c)
                                            (reportConsoleError (Just c)))
         else lift $ outputStrLn "No device found at that endpoint."

main :: IO ()
main = execParser opts >>= runOptions
    where opts = info (helper <*> options)
                      (mconcat [ fullDesc
                               , progDesc "The Flipper Console"
                               ])
