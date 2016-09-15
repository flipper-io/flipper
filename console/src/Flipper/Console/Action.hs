module Flipper.Console.Action where

import Data.Word

import Flipper.NVM
import Flipper.FS
import Flipper.IO
import Flipper.LED
import Flipper.USART

data ConsoleAction = Flash FilePath
                   | Install BundleID FilePath
                   | Launch String
                   | Reset
                   | Suspend
                   | Engage
                   | Format
                   | ConsoleCall Call
                   deriving (Eq, Ord, Show)

newtype BundleID = BundleID { unBundleID :: [String] }
                 deriving (Eq, Ord, Show)

data Call = ButtonCall ButtonAction
          | ConfigCall ConfigAction
          | IOCall IOAction
          | LEDCall LEDAction
          | NVMCall NVMAction
          | SPICall SPIAction
          | USARTCall USARTAction
          | USBCall USBAction
          deriving (Eq, Ord, Show)

data ButtonAction = ButtonRead
                  deriving (Eq, Ord, Show)

data ConfigAction = ConfigRead Word8
                  | ConfigWrite Word8 Word16
                  deriving (Eq, Ord, Show)

data IOAction = IODigitalDirection DigitalPin Direction
              | IODigitalRead DigitalPin
              | IODigitalWrite DigitalPin Bool
              | IOAnalogDirection AnalogPin Direction
              | IOAnalogRead AnalogPin
              | IOAnalogWrite AnalogPin Word16
              deriving (Eq, Ord, Show)

data LEDAction = LEDSetRGB RGB
               deriving (Eq, Ord, Show)

data NVMAction = NVMEnable
               | NVMDisable
               | NVMReset
               | NVMFormat
               | NVMAlloc Int
               | NVMRead FSHandle
               | NVMWrite FSHandle String
                deriving (Eq, Ord, Show)

data SPIAction = SPIEnable
               | SPIDisable
               | SPIRead
               | SPIWrite String
               deriving (Eq, Ord, Show)

data USARTAction = USARTEnable USART
                 | USARTDisable USART
                 | USARTRead USART
                 | USARTWrite USART String
                 deriving (Eq, Ord, Show)

data USBAction = USBEnable
               | USBDisable
               | USBRead
               | USBWrite String
               deriving (Eq, Ord, Show)
