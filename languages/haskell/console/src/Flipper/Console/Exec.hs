{-|
Module      : Flipper.Console.Exec
Description : Console action execution
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module is responsible for executing 'ConsoleAction's.
-}

module Flipper.Console.Exec (
    execConsoleAction
  , fcREPL
  ) where

import Control.Monad.Trans.Class

import qualified Data.ByteString as BS

import Flipper.MonadFlipper

import qualified Flipper.ADC         as ADC
import qualified Flipper.Button      as Button
import qualified Flipper.CPU         as CPU
import qualified Flipper.DAC         as DAC
import qualified Flipper.FS          as FS
import qualified Flipper.GPIO        as GPIO
import qualified Flipper.I2C         as I2C
import qualified Flipper.LED         as LED
import qualified Flipper.PWM         as PWM
import qualified Flipper.RTC         as RTC
import qualified Flipper.SPI         as SPI
import qualified Flipper.SWD         as SWD
import qualified Flipper.Temperature as Temp
import qualified Flipper.Timer       as Timer
import qualified Flipper.UART0       as UART0
import qualified Flipper.USART       as USART
import qualified Flipper.USB         as USB
import qualified Flipper.WDT         as WDT

import Flipper.Console.Action
import Flipper.Console.FC
import Flipper.Console.Flash
import Flipper.Console.Parsers

import System.Console.Haskeline

import qualified Text.Megaparsec as M

-- | Run a 'ConsoleAction' in the 'FC' monad.
execConsoleAction :: ConsoleAction -> FC ()
execConsoleAction (Flash f)       = execFlash f
execConsoleAction (Install m f)   = execInstall m f
execConsoleAction (Launch s)      = execLaunch s
execConsoleAction Reset           = execReset
execConsoleAction Suspend         = execSuspend
execConsoleAction Format          = execFormat
execConsoleAction (ConsoleCall c) = execCall c

-- | Implement this.
execInstall :: ModuleID -> FilePath -> FC ()
execInstall = undefined

-- | Implement this.
execLaunch :: String -> FC ()
execLaunch = undefined

-- | Reset the ATSAM4S16B.
execReset :: FC ()
execReset = CPU.reset

-- | Halt the ATSAM4S16B.
execSuspend :: FC ()
execSuspend = CPU.halt

-- | Format the device storage.
execFormat :: FC ()
execFormat = FS.format

-- | Run a standard module call in the 'FC' monad.
execCall :: Call -> FC ()
execCall (ADCCall a)    = execADCAction a
execCall (ButtonCall b) = execButtonAction b
execCall (DACCall d)    = execDACAction d
execCall (FSCall f)     = execFSAction f
execCall (GPIOCall g)   = execGPIOAction g
execCall (I2CCall i)    = execI2CAction i
execCall (LEDCall l)    = execLEDAction l
execCall (PWMCall p)    = execPWMAction p
execCall (RTCCall r)    = execRTCAction r
execCall (SPICall s)    = execSPIAction s
execCall (SWDCall s)    = execSWDAction s
execCall (TempCall t)   = execTempAction t
execCall (TimerCall t)  = execTimerAction t
execCall (UART0Call u)  = execUART0Action u
execCall (USARTCall u)  = execUSARTAction u
execCall (USBCall u)    = execUSBAction u
execCall (WDTCall w)    = execWDTAction w

-- | Run an ADC module call in the 'FC' monad.
execADCAction :: ADCAction -> FC ()
execADCAction ADCConfigure = ADC.configure >>= printFC

-- | Run a button module call in the 'FC' monad.
execButtonAction :: ButtonAction -> FC ()
execButtonAction ButtonConfigure = Button.configure >>= printFC
execButtonAction ButtonRead      = Button.read >>= printFC

-- | Run a DAC module call in the 'FC' monad.
execDACAction :: DACAction -> FC ()
execDACAction DACConfigure = DAC.configure >>= printFC

-- | Run a file system module call in the 'FC' monad.
execFSAction :: FSAction -> FC ()
execFSAction FSConfigure      = FS.configure >>= printFC
execFSAction (FSCreate fn)    = FS.create fn
execFSAction (FSDelete fn)    = FS.delete fn
execFSAction (FSSize fn)      = FS.size fn >>= printFC
execFSAction (FSOpen fn o)    = FS.open fn o
execFSAction (FSPushString p) = FS.push p
execFSAction FSPullString     = FS.pull >>= printCStringFC
execFSAction FSClose          = FS.close

-- | Run a GPIO module call in the 'FC' monad.
execGPIOAction :: GPIOAction -> FC ()
execGPIOAction GPIOConfigure              = GPIO.configure >>= printFC
execGPIOAction (GPIODigitalDirection p d) = GPIO.digitalDirection p d
execGPIOAction (GPIODigitalRead p)        = GPIO.digitalRead p >>= printFC
execGPIOAction (GPIODigitalWrite p v)     = GPIO.digitalWrite p v
execGPIOAction (GPIOAnalogDirection p d)  = GPIO.analogDirection p d
execGPIOAction (GPIOAnalogRead p)         = GPIO.analogRead p >>= printFC
execGPIOAction (GPIOAnalogWrite p v)      = GPIO.analogWrite p v

-- | Run an I2C module call in the 'FC' monad.
execI2CAction :: I2CAction -> FC ()
execI2CAction I2CConfigure = I2C.configure >>= printFC

-- | Run an LED module call in the 'FC' monad.
execLEDAction :: LEDAction -> FC ()
execLEDAction LEDConfigure  = LED.configure >>= printFC
execLEDAction (LEDSetRGB c) = LED.setRGB c

-- | Run a PWM module call in the 'FC' monad.
execPWMAction :: PWMAction -> FC ()
execPWMAction PWMConfigure = PWM.configure >>= printFC

-- | Run a PWM module call in the 'FC' monad.
execRTCAction :: RTCAction -> FC ()
execRTCAction RTCConfigure = RTC.configure >>= printFC

-- | Run an SPI module call in the 'FC' monad.
execSPIAction :: SPIAction -> FC ()
execSPIAction SPIConfigure           = SPI.configure >>= printFC
execSPIAction SPIEnable              = SPI.enable
execSPIAction SPIDisable             = SPI.disable
execSPIAction SPIRead                = SPI.pull >>= printCStringFC
execSPIAction (SPIWriteFromString s) = SPI.push s
execSPIAction (SPIWriteFromFile fp)  = liftFC (BS.readFile fp) >>= SPI.push

-- | Run an SWD module call in the 'FC' monad.
execSWDAction :: SWDAction -> FC ()
execSWDAction SWDConfigure = SWD.configure >>= printFC

-- | Run a temperature module call in the 'FC' monad.
execTempAction :: TempAction -> FC ()
execTempAction TempConfigure = Temp.configure >>= printFC

-- | Run a timer module call in the 'FC' monad.
execTimerAction :: TimerAction -> FC ()
execTimerAction TimerConfigure = Timer.configure >>= printFC

-- | Run a UART0 module call in the 'FC' monad.
execUART0Action :: UART0Action -> FC ()
execUART0Action UART0Configure           = UART0.configure >>= printFC
execUART0Action UART0Enable              = UART0.enable
execUART0Action UART0Disable             = UART0.disable
execUART0Action UART0Read                = UART0.pull >>= printCStringFC
execUART0Action (UART0WriteFromString s) = UART0.push s
execUART0Action (UART0WriteFromFile fp)  = liftFC (BS.readFile fp) >>= UART0.push

-- | Run a USART module call in the 'FC' monad.
execUSARTAction :: USARTAction -> FC ()
execUSARTAction USARTConfigure           = USART.configure >>= printFC
execUSARTAction USARTEnable              = USART.enable
execUSARTAction USARTDisable             = USART.disable
execUSARTAction USARTRead                = USART.pull >>= printCStringFC
execUSARTAction (USARTWriteFromString s) = USART.push s
execUSARTAction (USARTWriteFromFile fp)  = liftFC (BS.readFile fp) >>= USART.push

-- | Run a USB module call in the 'FC' monad.
execUSBAction :: USBAction -> FC ()
execUSBAction USBConfigure = USB.configure >>= printFC

-- | Run a WDT module call in the 'FC' monad.
execWDTAction :: WDTAction -> FC ()
execWDTAction WDTConfigure = WDT.configure >>= printFC
execWDTAction WDTFire      = WDT.fire

-- | The top-level console REPL. Parses input lines into 'ConsoleAction's and
--   executes them until the user hits Ctrl-D.
fcREPL :: FC ()
fcREPL = do
    i <- lift $ getInputLine "flipper>"
    case i of Nothing  -> return ()
              (Just l) -> execUserInput l *> fcREPL

-- | Parse a line of user input. On a successful parse, run the 'ConsoleAction'
--   and pretty print the output and/or error. Otherwise print the parse error.
--   TODO: pretty print the parse error.
execUserInput :: String -> FC ()
execUserInput l = case M.runParser parseConsoleAction "<interactive>" l of
    (Left e)  -> lift (outputStrLn (M.parseErrorPretty e))
    (Right c) -> catchFlipper (execConsoleAction c)
                              (reportConsoleError (Just c))
