module Main where

import Control.Monad

import Control.Monad.Trans.Class

import Flipper
import Flipper.MonadFlipper

import Flipper.Console.Action
import Flipper.Console.Exec
import Flipper.Console.Parsers
import Flipper.Console.Options

import Options.Applicative

import System.Console.Haskeline

runOptions :: Options -> IO ()
runOptions (Options e o) = (void . runFC) $ do
    s <- catchFlipper (attach e) (\x -> reportConsoleError Nothing x >> return False)
    case s of False -> lift $ outputStrLn "No device found at that endpoint."
              True  -> case o of Nothing  -> fcREPL
                                 (Just c) -> catchFlipper (execConsoleAction c) (reportConsoleError (Just c))

main :: IO ()
main = execParser opts >>= runOptions
    where opts = info (helper <*> options) $ mconcat [ fullDesc
                                                     , progDesc "The Flipper Console"
                                                     ]
