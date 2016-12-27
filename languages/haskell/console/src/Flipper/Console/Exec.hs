module Flipper.Console.Exec where

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

execConsoleAction :: ConsoleAction -> FC ()
execConsoleAction (Flash f)       = execFlash f
execConsoleAction (Install m f)   = execInstall m f
execConsoleAction (Launch s)      = execLaunch s
execConsoleAction Reset           = execReset
execConsoleAction Suspend         = execSuspend
execConsoleAction Format          = execFormat
execConsoleAction (ConsoleCall c) = execCall c

execInstall :: ModuleID -> FilePath -> FC ()
execInstall = undefined

execLaunch :: String -> FC ()
execLaunch = undefined

execReset :: FC ()
execReset = CPU.reset

execSuspend :: FC ()
execSuspend = CPU.halt

execFormat :: FC ()
execFormat = FS.format

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

execADCAction :: ADCAction -> FC ()
execADCAction ADCConfigure = ADC.configure >>= printFC

execButtonAction :: ButtonAction -> FC ()
execButtonAction ButtonConfigure = Button.configure >>= printFC
execButtonAction ButtonRead      = Button.read >>= printFC

execDACAction :: DACAction -> FC ()
execDACAction DACConfigure = DAC.configure >>= printFC

execFSAction :: FSAction -> FC ()
execFSAction FSConfigure      = FS.configure >>= printFC
execFSAction (FSCreate fn)    = FS.create fn
execFSAction (FSDelete fn)    = FS.delete fn
execFSAction (FSSize fn)      = FS.size fn >>= printFC
execFSAction (FSOpen fn o)    = FS.open fn o
execFSAction (FSPushString p) = FS.push p
execFSAction FSPullString     = FS.pull >>= printCStringFC
execFSAction FSClose          = FS.close

execGPIOAction :: GPIOAction -> FC ()
execGPIOAction GPIOConfigure              = GPIO.configure >>= printFC
execGPIOAction (GPIODigitalDirection p d) = GPIO.digitalDirection p d
execGPIOAction (GPIODigitalRead p)        = GPIO.digitalRead p >>= printFC
execGPIOAction (GPIODigitalWrite p v)     = GPIO.digitalWrite p v
execGPIOAction (GPIOAnalogDirection p d)  = GPIO.analogDirection p d
execGPIOAction (GPIOAnalogRead p)         = GPIO.analogRead p >>= printFC
execGPIOAction (GPIOAnalogWrite p v)      = GPIO.analogWrite p v

execI2CAction :: I2CAction -> FC ()
execI2CAction I2CConfigure = I2C.configure >>= printFC

execLEDAction :: LEDAction -> FC ()
execLEDAction LEDConfigure  = LED.configure >>= printFC
execLEDAction (LEDSetRGB c) = LED.setRGB c

execPWMAction :: PWMAction -> FC ()
execPWMAction PWMConfigure = PWM.configure >>= printFC

execRTCAction :: RTCAction -> FC ()
execRTCAction RTCConfigure = RTC.configure >>= printFC

execSPIAction :: SPIAction -> FC ()
execSPIAction SPIConfigure           = SPI.configure >>= printFC
execSPIAction SPIEnable              = SPI.enable
execSPIAction SPIDisable             = SPI.disable
execSPIAction SPIRead                = SPI.pull >>= printCStringFC
execSPIAction (SPIWriteFromString s) = SPI.push s
execSPIAction (SPIWriteFromFile fp)  = liftFC (BS.readFile fp) >>= SPI.push

execSWDAction :: SWDAction -> FC ()
execSWDAction SWDConfigure = SWD.configure >>= printFC

execTempAction :: TempAction -> FC ()
execTempAction TempConfigure = Temp.configure >>= printFC

execTimerAction :: TimerAction -> FC ()
execTimerAction TimerConfigure = Timer.configure >>= printFC

execUART0Action :: UART0Action -> FC ()
execUART0Action UART0Configure           = UART0.configure >>= printFC
execUART0Action UART0Enable              = UART0.enable
execUART0Action UART0Disable             = UART0.disable
execUART0Action UART0Read                = UART0.pull >>= printCStringFC
execUART0Action (UART0WriteFromString s) = UART0.push s
execUART0Action (UART0WriteFromFile fp)  = liftFC (BS.readFile fp) >>= UART0.push

execUSARTAction :: USARTAction -> FC ()
execUSARTAction USARTConfigure           = USART.configure >>= printFC
execUSARTAction USARTEnable              = USART.enable
execUSARTAction USARTDisable             = USART.disable
execUSARTAction USARTRead                = USART.pull >>= printCStringFC
execUSARTAction (USARTWriteFromString s) = USART.push s
execUSARTAction (USARTWriteFromFile fp)  = liftFC (BS.readFile fp) >>= USART.push

execUSBAction :: USBAction -> FC ()
execUSBAction USBConfigure = USB.configure >>= printFC

execWDTAction :: WDTAction -> FC ()
execWDTAction WDTConfigure = WDT.configure >>= printFC

fcREPL :: FC ()
fcREPL = do
    i <- lift $ getInputLine "flipper>"
    case i of Nothing  -> return ()
              (Just l) -> execUserInput l *> fcREPL

execUserInput :: String -> FC ()
execUserInput l = case M.runParser parseConsoleAction "<interactive>" l of
    (Left e)  -> lift (outputStrLn (show e))
    (Right c) -> catchFlipper (execConsoleAction c)
                              (reportConsoleError (Just c))
