module Flipper.Console.Exec where

import Control.Monad.Trans.Class

import Flipper

import Flipper.Error
import Flipper.MonadFlipper

import qualified Flipper.Button as Button
import qualified Flipper.Config as Config
import qualified Flipper.FDL    as FDL
import qualified Flipper.FS     as FS
import qualified Flipper.IO     as IO
import qualified Flipper.LED    as LED
import qualified Flipper.NVM    as NVM
import qualified Flipper.SAM    as SAM
import qualified Flipper.SPI    as SPI
import qualified Flipper.USART  as USART
import qualified Flipper.USB    as USB

import Flipper.Console.Action
import Flipper.Console.Parsers
import Flipper.Console.Error

import System.Console.Haskeline

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

execConsoleAction :: ConsoleAction -> FC ()
execConsoleAction (Flash f)       = execFlash f
execConsoleAction (Install b f)   = execInstall b f
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
execLaunch = FDL.launch . FDL.bundleKey

execReset :: FC ()
execReset = SAM.reset

execSuspend :: FC ()
execSuspend = SAM.suspend

execEngage :: FC ()
execEngage = SAM.engage

execFormat :: FC ()
execFormat = NVM.format

execCall :: Call -> FC ()
execCall (ButtonCall b) = execButtonAction b
execCall (ConfigCall c) = execConfigAction c
execCall (IOCall i)     = execIOAction i
execCall (LEDCall l)    = execLEDAction l
execCall (NVMCall n)    = execNVMAction n
execCall (SPICall s)    = execSPIAction s
execCall (USARTCall u)  = execUSARTAction u
execCall (USBCall u)    = execUSBAction u

execButtonAction :: ButtonAction -> FC ()
execButtonAction ButtonRead = Button.read >>= printFC

execConfigAction :: ConfigAction -> FC ()
execConfigAction (ConfigRead k)    = Config.read k >>= printFC
execConfigAction (ConfigWrite k v) = Config.write k v

execIOAction :: IOAction -> FC ()
execIOAction (IODigitalDirection p d) = IO.digitalDirection p d
execIOAction (IODigitalRead p)        = IO.digitalRead p >>= printFC
execIOAction (IODigitalWrite p v)     = IO.digitalWrite p v
execIOAction (IOAnalogDirection p d)  = IO.analogDirection p d
execIOAction (IOAnalogRead p)         = IO.analogRead p >>= printFC
execIOAction (IOAnalogWrite p v)      = IO.analogWrite p v

execLEDAction :: LEDAction -> FC ()
execLEDAction (LEDSetRGB c) = LED.setRGB c

execNVMAction :: NVMAction -> FC ()
execNVMAction NVMEnable      = NVM.enable
execNVMAction NVMDisable     = NVM.disable
execNVMAction NVMReset       = NVM.reset
execNVMAction NVMFormat      = NVM.format
execNVMAction (NVMAlloc s)   = NVM.alloc (fromIntegral s) >>= printFC
execNVMAction (NVMRead h)    = NVM.pull h >>= printCStringFC
execNVMAction (NVMWrite h s) = NVM.pushHandle s h

execSPIAction :: SPIAction -> FC ()
execSPIAction SPIEnable    = SPI.enable
execSPIAction SPIDisable   = SPI.disable
execSPIAction SPIRead      = SPI.pull >>= printCStringFC
execSPIAction (SPIWrite s) = SPI.push s

execUSARTAction :: USARTAction -> FC ()
execUSARTAction (USARTEnable u)  = USART.enable u
execUSARTAction (USARTDisable u) = USART.disable u
execUSARTAction (USARTRead u)    = USART.pull u >>= printCStringFC
execUSARTAction (USARTWrite u s) = USART.push u s

execUSBAction :: USBAction -> FC ()
execUSBAction USBEnable    = USB.enable
execUSBAction USBDisable   = USB.disable
execUSBAction USBRead      = USB.pull >>= printCStringFC
execUSBAction (USBWrite s) = USB.push s

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
