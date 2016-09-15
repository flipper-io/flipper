module Flipper.Console.Error where

import Flipper.Error
import Flipper.Console.Action

consoleError :: Maybe ConsoleAction -> FlipperError -> String
consoleError _ _ = "error"
