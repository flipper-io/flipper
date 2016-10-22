module Flipper.Console.Action where

import Data.Word

import Flipper.GPIO
import Flipper.LED

-- | Top-level console action.
data ConsoleAction = -- | Flash firmware from a file to the device.
                     Flash FilePath
                     -- | Install a module from a file on to the device.
                   | Install ModuleID FilePath
                     -- | Launch a program on the device.
                   | Launch String
                     -- | Reset the device.
                   | Reset
                     -- | Suspend the device.
                   | Suspend
                     -- | Format the flash storage on the device.
                   | Format
                     -- | Call a standard module function.
                   | ConsoleCall Call
                   deriving (Eq, Ord, Show)

newtype ModuleID = ModuleID { unModuleID :: [String] }
                 deriving (Eq, Ord, Show)

-- | A standard module function call.
data Call = -- | Button module call.
            ButtonCall ButtonAction
            -- | File system module call.
          | FSCall FSAction
            -- | GPIO module call.
          | GPIOCall GPIOAction
            -- | LED module call.
          | LEDCall LEDAction
            -- | SPI module call.
          | SPICall SPIAction
            -- | USART
          | USARTCall USARTAction
          deriving (Eq, Ord, Show)

data ButtonAction = ButtonRead
                  deriving (Eq, Ord, Show)

data FSAction = FSCreateFromString String String
              | FSCreateFromFile String FilePath
              | FSRemove String
              | FSRename String String
              deriving (Eq, Ord, Show)

data GPIOAction = GPIODigitalDirection DigitalPin Direction
                | GPIODigitalRead DigitalPin
                | GPIODigitalWrite DigitalPin Bool
                | GPIOAnalogDirection AnalogPin Direction
                | GPIOAnalogRead AnalogPin
                | GPIOAnalogWrite AnalogPin Word16
                deriving (Eq, Ord, Show)

data LEDAction = LEDSetRGB RGB
               deriving (Eq, Ord, Show)

data SPIAction = SPIEnable
               | SPIDisable
               | SPIRead
               | SPIWriteFromString String
               | SPIWriteFromFile FilePath
               deriving (Eq, Ord, Show)

data USARTAction = USARTEnable
                 | USARTDisable
                 | USARTRead
                 | USARTWriteFromString String
                 | USARTWriteFromFile String
                 deriving (Eq, Ord, Show)
