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

-- | Deprecate this.
newtype ModuleID = ModuleID { unModuleID :: [String] }
                 deriving (Eq, Ord, Show)

-- | A standard module function call.
data Call = -- | ADC module call.
            ADCCall ADCAction
            -- | Button module call.
          | ButtonCall ButtonAction
            -- | DAC module call.
          | DACCall DACAction
            -- | File system module call.
          | FSCall FSAction
            -- | GPIO module call.
          | GPIOCall GPIOAction
            -- | I2C module call.
          | I2CCall I2CAction
            -- | LED module call.
          | LEDCall LEDAction
            -- | PWM module call.
          | PWMCall PWMAction
            -- | RTC module call.
          | RTCCall RTCAction
            -- | SPI module call.
          | SPICall SPIAction
            -- | SWD module call.
          | SWDCall SWDAction
            -- | Temperature module call.
          | TempCall TempAction
            -- | Timer module call.
          | TimerCall TimerAction
            -- | UART0 module call.
          | UART0Call UART0Action
            -- | USART module call.
          | USARTCall USARTAction
            -- | USB module call.
          | USBCall USBAction
            -- | WDT module call.
          | WDTCall WDTAction
          deriving (Eq, Ord, Show)

-- | ADC module call.
data ADCAction = -- | Configure the ADC module.
                 ADCConfigure
               deriving (Eq, Ord, Show)

-- | Button module call.
data ButtonAction = -- | Configure the button module.
                    ButtonConfigure
                    -- | Read the button state.
                  | ButtonRead
                  deriving (Eq, Ord, Show)

-- | DAC module call.
data DACAction = -- | Configure the DAC module.
                 DACConfigure
               deriving (Eq, Ord, Show)

-- | File system module call.
data FSAction = -- | Configure the file system module.
                FSConfigure
                -- | Create a file.
              | FSCreate String
                -- | Delete a file.
              | FSDelete String
                -- | Query file size.
              | FSSize String
                -- | Open a file for streaming I/O.
              | FSOpen String Word32
                -- | Write a null-terminated string to the open file.
              | FSPushString String
                -- | Read a null-terminated string to the open file.
              | FSPullString
                -- | Close the open file.
              | FSClose
              deriving (Eq, Ord, Show)

-- | GPIO module call.
data GPIOAction = -- | Configure the GPIO module.
                  GPIOConfigure
                  -- | Set digital pin I/O direction.
                | GPIODigitalDirection DigitalPin Direction
                  -- | Read digital pin value.
                | GPIODigitalRead DigitalPin
                  -- | Write digital pin value.
                | GPIODigitalWrite DigitalPin Bool
                  -- | Set analog pin I/O direction.
                | GPIOAnalogDirection AnalogPin Direction
                  -- | Read analog pin value.
                | GPIOAnalogRead AnalogPin
                  -- | Write analog pin value.
                | GPIOAnalogWrite AnalogPin Word16
                deriving (Eq, Ord, Show)

-- | I2C module call.
data I2CAction = -- | Configure the I2C module.
                 I2CConfigure
               deriving (Eq, Ord, Show)

-- | LED module call.
data LEDAction = -- | Configure the LED module.
                 LEDConfigure
                 -- | Set the LED color.
               | LEDSetRGB RGB
               deriving (Eq, Ord, Show)

-- | PWM module call.
data PWMAction = -- | Configure the PWM module.
                 PWMConfigure
               deriving (Eq, Ord, Show)

-- | RTC module call.
data RTCAction = -- | Configure the RTC module.
                 RTCConfigure
               deriving (Eq, Ord, Show)

-- | SPI module call.
data SPIAction = -- | Configure the SPI module.
                 SPIConfigure
                 -- | Enable the SPI bus.
               | SPIEnable
                 -- | Disable the SPI bus.
               | SPIDisable
                 -- | Initiate a read from the SPI bus.
               | SPIRead
                 -- | Write a null-terminated string to the SPI bus.
               | SPIWriteFromString String
                 -- | Write data from a file to the SPI bus.
               | SPIWriteFromFile FilePath
               deriving (Eq, Ord, Show)

-- | SWD module call.
data SWDAction = -- | Configure the SWD module.
                 SWDConfigure
               deriving (Eq, Ord, Show)

-- | Temperature module call.
data TempAction = -- | Configure the temperature module.
                  TempConfigure
                deriving (Eq, Ord, Show)

-- | Timer module call.
data TimerAction = -- | Configure the timer module.
                   TimerConfigure
                 deriving (Eq, Ord, Show)

-- | UART0 module call.
data UART0Action = -- | Configure the UART0 module.
                   UART0Configure
                   -- | Enable the UART0.
                 | UART0Enable
                   -- | Disable the UART0.
                 | UART0Disable
                   -- | Initiate a UART0 read.
                 | UART0Read
                   -- | Write a null-terminated to the UART0 bus.
                 | UART0WriteFromString String
                   -- | Write data from a file to the UART0 bus.
                 | UART0WriteFromFile String
                 deriving (Eq, Ord, Show)

-- | USART moduel call.
data USARTAction = -- | Configure the USART.
                   USARTConfigure
                   -- | Enable the USART.
                 | USARTEnable
                   -- | Disable the USART.
                 | USARTDisable
                   -- | Initiate a USART read.
                 | USARTRead
                   -- | Write a null-terminated string to the USART bus.
                 | USARTWriteFromString String
                   -- | Write data from a file to the USART bus.
                 | USARTWriteFromFile String
                 deriving (Eq, Ord, Show)

-- | USB module call.
data USBAction = -- | Configure the USB module.
                 USBConfigure
               deriving (Eq, Ord, Show)

-- | WDT moduel call.
data WDTAction = -- | Configure the WDT module.
                 WDTConfigure
               deriving (Eq, Ord, Show)
