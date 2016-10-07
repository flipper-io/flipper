module Flipper.Console.FC where

import Control.Monad.Trans.Class

import Flipper.Error
import Flipper.MonadFlipper

import Flipper.Console.Action
import Flipper.Console.Error

import System.Console.Haskeline

type FC = FlipperT (InputT IO)

liftFC :: IO a -> FC a
liftFC = lift . lift

runFC :: FC a -> IO (Either FlipperException a)
runFC = (runInputT defaultSettings) . runFlipperT

printFC :: Show a => a -> FC ()
printFC = lift . outputStrLn . show

printCStringFC :: Either String String -> FC ()
printCStringFC (Left e)  = lift $ outputStrLn e
printCStringFC (Right v) = lift $ outputStrLn v

reportConsoleError :: Maybe ConsoleAction -> FlipperException -> FC ()
reportConsoleError c (FlipperException e) =
    lift (outputStrLn (consoleError c e))
