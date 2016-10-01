module Flipper.Console.Exec where

import Control.Monad.Trans.Class

import qualified Data.ByteString as BS

import Flipper

import Flipper.Error
import Flipper.MonadFlipper

import qualified Flipper.Buffer as B
import qualified Flipper.Button as Button
import qualified Flipper.FS     as FS
import qualified Flipper.GPIO   as GPIO
import qualified Flipper.LED    as LED
import qualified Flipper.CPU    as CPU
import qualified Flipper.SPI    as SPI
import qualified Flipper.UART   as UART

import Flipper.Console.Action
import Flipper.Console.Parsers
import Flipper.Console.Error

import System.Console.Haskeline

import System.IO

import qualified Text.Megaparsec        as M
import qualified Text.Megaparsec.String as M

type FC = FlipperT (InputT IO)

runFC :: FC a -> IO (Either FlipperException a)
runFC = (runInputT defaultSettings) . runFlipperT

printFC :: Show a => a -> FC ()
printFC = lift . outputStrLn . show

printCStringFC :: Either String String -> FC ()
printCStringFC (Left e)  = lift $ outputStrLn e
printCStringFC (Right v) = lift $ outputStrLn v

withFileFC :: FilePath -> (Buffer -> FC a) -> FC a
withFileFC fp f = withFile fp ReadMode $ \h ->
    (Buffer.fromByteString <$> lift (BS.readFile h)) >>= f

execConsoleAction :: ConsoleAction -> FC ()
execConsoleAction (Flash f)       = execFlash f
execConsoleAction (Install m f)   = execInstall m f
execConsoleAction (Launch s)      = execLaunch s
execConsoleAction Reset           = execReset
execConsoleAction Suspend         = execSuspend
execConsoleAction Engage          = execEngage
execConsoleAction Format          = execFormat
execConsoleAction (ConsoleCall c) = execCall c

execFlash :: FilePath -> FC ()
execFlash = undefined

execInstall :: BundleID -> FilePath -> FC ()
execInstall = undefined

execLaunch :: String -> FC ()
execLaunch = undefined

execReset :: FC ()
execReset = CPU.reset

execSuspend :: FC ()
execSuspend = CPU.halt

execEngage :: FC ()
execEngage = CPU.power True

execFormat :: FC ()
execFormat = FS.format

execCall :: Call -> FC ()
execCall (ButtonCall b) = execButtonAction b
execCall (FSCall f)     = execFSAction f
execCall (GPIOCall g)   = execGPIOAction g
execCall (LEDCall l)    = execLEDAction l
execCall (SPICall s)    = execSPIAction s
execCall (UARTCall u)   = execUSARTAction u

execButtonAction :: ButtonAction -> FC ()
execButtonAction ButtonRead = Button.read >>= printFC

execGPIOAction :: GPIOAction -> FC ()
execGPIOAction (GPIODigitalDirection p d) = GPIO.digitalDirection p d
execGPIOAction (GPIODigitalRead p)        = GPIO.digitalRead p >>= printFC
execGPIOAction (GPIODigitalWrite p v)     = GPIO.digitalWrite p v
execGPIOAction (GPIOAnalogDirection p d)  = GPIO.analogDirection p d
execGPIOAction (GPIOAnalogRead p)         = GPIO.analogRead p >>= printFC
execGPIOAction (GPIOAnalogWrite p v)      = GPIO.analogWrite p v

execLEDAction :: LEDAction -> FC ()
execLEDAction (LEDSetRGB c) = LED.setRGB c

execSPIAction :: SPIAction -> FC ()
execSPIAction SPIEnable                 = SPI.enable
execSPIAction SPIDisable                = SPI.disable
execSPIAction (SPIRead i)               = SPI.pull i >>= printCStringFC
execSPIAction (SPIWriteFromString s)    = SPI.push s
execSPIAction (SPIWriteFromFilePath fp) = withFileFC fp SPI.push

execUARTAction :: UARTAction -> FC ()
execUARTAction UARTEnable                 = UART.enable
execUARTAction UARTDisable                = UART.disable
execUARTAction (UARTRead i)               = UART.pull i >>= printCStringFC
execUARTAction (UARTWriteFromString s)    = UART.push s
execUARTAction (UARTWriteFromFilePath fp) = withFileFC fp UART.push

fcREPL :: FC ()
fcREPL = do
    i <- lift $ getInputLine "flipper>"
    case i of Nothing  -> return ()
              (Just l) -> execUserInput l *> fcREPL

execUserInput :: String -> FC ()
execUserInput l = case M.runParser parseConsoleAction "<interactive>" l
                  of (Left e)  -> lift (outputStrLn (show e))
                     (Right c) -> catchFlipper (execConsoleAction c) (reportConsoleError (Just c))

reportConsoleError :: Maybe ConsoleAction -> FlipperException -> FC ()
reportConsoleError c (FlipperException e) = lift (outputStrLn (consoleError c e))
