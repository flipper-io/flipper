module Flipper.Console.Options where

import Control.Applicative

import Data.Foldable
import Data.Word

import Flipper
import Flipper.FS
import Flipper.IO (DigitalPin(..), AnalogPin(..), Direction(..))
import Flipper.LED
import Flipper.USART

import Flipper.Console.Action
import Flipper.Console.Parsers

import Options.Applicative hiding (action)

import qualified Text.Megaparsec        as M
import qualified Text.Megaparsec.String as M

data Options = Options {
    optEndpoint :: Endpoint
  , optOneoff   :: Maybe ConsoleAction
  } deriving (Eq, Ord, Show)

options :: Parser Options
options = Options <$> endpoint <*> (optional action)

endpoint :: Parser Endpoint
endpoint = option (readParser parseEndpoint) opt
    where opt = mconcat [ long "endpoint"
                        , short 'e'
                        , metavar "ENDPOINT"
                        , help "The endpoint via which Flipper will be attached."
                        , value (USB Nothing)
                        ]

readParser :: M.Parser a -> ReadM a
readParser p = eitherReader (showLeft . M.runParser p "")
    where showLeft (Right x) = (Right x)
          showLeft (Left e)  = (Left (show e))

action :: Parser ConsoleAction
action = builtin <|> (ConsoleCall <$> call)
    where builtin = subparser $ mconcat [ command "flash" flash
                                        , command "install" install
                                        , command "launch" launch
                                        , command "reset" reset
                                        , command "suspend" suspend
                                        , command "engage" engage
                                        , command "format" format
                                        ]

flash :: ParserInfo ConsoleAction
flash = info (Flash <$> flashP) flashI
    where flashI = mconcat [ fullDesc
                           ]
          flashP = strOption $ mconcat [ metavar "FIRMWARE"
                                       , help "Firmware file image."
                                       ]

install :: ParserInfo ConsoleAction
install = info (Install <$> bundleID <*> installP) installI
    where installI = mconcat [ fullDesc
                             ]
          installP = strOption $ mconcat [ metavar "MODULE_FILE"
                                         , help "Module file image."
                                         ]

launch :: ParserInfo ConsoleAction
launch = info (Launch <$> launchP) launchI
    where launchI = mconcat [ fullDesc
                            ]
          launchP = strOption $ mconcat [ metavar "MODULE"
                                        , help "Module name."
                                        ]

reset :: ParserInfo ConsoleAction
reset = info (pure Reset) resetI
    where resetI = mconcat [ fullDesc
                           ]

suspend :: ParserInfo ConsoleAction
suspend = info (pure Suspend) suspendI
    where suspendI = mconcat [ fullDesc
                             ]

engage :: ParserInfo ConsoleAction
engage = info (pure Engage) engageI
    where engageI = mconcat [ fullDesc
                            ]

format :: ParserInfo ConsoleAction
format = info (pure Format) formatI
    where formatI = mconcat [ fullDesc
                            ]

call :: Parser Call
call = subparser $ mconcat [ command "button" (ButtonCall <$> button)
                           , command "config" (ConfigCall <$> config)
                           , command "io" (IOCall <$> io)
                           , command "led" (LEDCall <$> led)
                           , command "nvm" (NVMCall <$> nvm)
                           , command "spi" (SPICall <$> spi)
                           , command "usart0" (USARTCall <$> (usart USART0))
                           , command "usart1" (USARTCall <$> (usart USART1))
                           , command "dbgu" (USARTCall <$> (usart DBGU))
                           , command "usb" (USBCall <$> usb)
                           ]

button :: ParserInfo ButtonAction
button = info buttonRead buttonI
    where buttonI = mconcat [ fullDesc
                            ]

config :: ParserInfo ConfigAction
config = info (configRead <|> configWrite) configI
    where configI = mconcat [ fullDesc
                            ]

io :: ParserInfo IOAction
io = info (ioDirection <|> ioRead <|> ioWrite) ioI
    where ioI = mconcat  [ fullDesc
                         ]

led :: ParserInfo LEDAction
led = info ledRGB ledI
    where ledI = mconcat [ fullDesc
                         ]

nvm :: ParserInfo NVMAction
nvm = info nvmP nvmI
    where nvmI = mconcat [ fullDesc
                         ]
          nvmP = asum [ nvmEnable
                      , nvmDisable
                      , nvmReset
                      , nvmFormat
                      , nvmAlloc
                      , nvmRead
                      , nvmWrite
                      ]

spi :: ParserInfo SPIAction
spi = info spiP spiI
    where spiI = mconcat [ fullDesc
                         ]
          spiP = asum [ spiEnable
                      , spiDisable
                      , spiRead
                      , spiWrite
                      ]

usart :: USART -> ParserInfo USARTAction
usart u = info (usartP <*> pure u) usartI
    where usartI = mconcat [ fullDesc
                           ]
          usartP = asum [ usartEnable
                        , usartDisable
                        , usartRead
                        , usartWrite
                        ]

usb :: ParserInfo USBAction
usb = info usbP usbI
    where usbI = mconcat [ fullDesc
                         ]
          usbP = asum [ usbEnable
                      , usbDisable
                      , usbRead
                      , usbWrite
                      ]

buttonRead :: Parser ButtonAction
buttonRead = subparser (command "read" (info (pure ButtonRead) mempty))

configRead :: Parser ConfigAction
configRead = subparser (command "read" (info (ConfigRead <$> word8) mempty))

configWrite :: Parser ConfigAction
configWrite = subparser (command "write" (info (ConfigWrite <$> word8 <*> word16) mempty))

ioDirection :: Parser IOAction
ioDirection = subparser (command "direction" (info (digitalDirection <|> analogDirection) mempty))

digitalPin :: Parser DigitalPin
digitalPin = argument (readParser parseDigitalPin) mempty

analogPin :: Parser AnalogPin
analogPin = argument (readParser parseAnalogPin) mempty

direction :: Parser Direction
direction = argument (readParser parseDirection) mempty

digitalDirection :: Parser IOAction
digitalDirection = IODigitalDirection <$> digitalPin <*> direction

analogDirection :: Parser IOAction
analogDirection = IOAnalogDirection <$> analogPin <*> direction

ioRead :: Parser IOAction
ioRead = subparser (command "read" (info (digitalRead <|> analogRead) mempty))

digitalRead :: Parser IOAction
digitalRead = IODigitalRead <$> digitalPin

analogRead :: Parser IOAction
analogRead = IOAnalogRead <$> analogPin

ioWrite :: Parser IOAction
ioWrite = subparser (command "write" (info (digitalWrite <|> analogWrite) mempty))

digitalWrite :: Parser IOAction
digitalWrite = IODigitalWrite <$> digitalPin <*> bool

analogWrite :: Parser IOAction
analogWrite = IOAnalogWrite <$> analogPin <*> word16

ledRGB :: Parser LEDAction
ledRGB = subparser (command "rgb" (info (LEDSetRGB <$> rgb) mempty))

nvmEnable :: Parser NVMAction
nvmEnable = subparser (command "enable" (info (pure NVMEnable) mempty))

nvmDisable :: Parser NVMAction
nvmDisable = subparser (command "disable" (info (pure NVMDisable) mempty))

nvmReset :: Parser NVMAction
nvmReset = subparser (command "reset" (info (pure NVMDisable) mempty))

nvmFormat :: Parser NVMAction
nvmFormat = subparser (command "format" (info (pure NVMDisable) mempty))

nvmAlloc :: Parser NVMAction
nvmAlloc = subparser (command "alloc" (info (NVMAlloc <$> allocSize) mempty))

nvmRead :: Parser NVMAction
nvmRead = subparser (command "read" (info (NVMRead <$> fshandle) mempty))

nvmWrite :: Parser NVMAction
nvmWrite = subparser (command "write" (info (NVMWrite <$> fshandle <*> strOption mempty) mempty))

spiEnable :: Parser SPIAction
spiEnable = subparser (command "enable" (info (pure SPIEnable) mempty))

spiDisable :: Parser SPIAction
spiDisable = subparser (command "disable" (info (pure SPIDisable) mempty))

spiRead :: Parser SPIAction
spiRead = subparser (command "read" (info (pure SPIRead) mempty))

spiWrite :: Parser SPIAction
spiWrite = subparser (command "write" (info (SPIWrite <$> strOption mempty) mempty))

usartEnable :: Parser (USART -> USARTAction)
usartEnable = subparser (command "enable" (info (pure USARTEnable) mempty))

usartDisable :: Parser (USART -> USARTAction)
usartDisable = subparser (command "disable" (info (pure USARTDisable) mempty))

usartRead :: Parser (USART -> USARTAction)
usartRead = subparser (command "read" (info (pure USARTRead) mempty))

usartWrite :: Parser (USART -> USARTAction)
usartWrite = subparser (command "write" (info (((\s -> (\u -> USARTWrite u s))) <$> strOption mempty) mempty))

usbEnable :: Parser USBAction
usbEnable = subparser (command "enable" (info (pure USBEnable) mempty))

usbDisable :: Parser USBAction
usbDisable = subparser (command "disable" (info (pure USBDisable) mempty))

usbRead :: Parser USBAction
usbRead = subparser (command "read" (info (pure USBRead) mempty))

usbWrite :: Parser USBAction
usbWrite = subparser (command "write" (info (USBWrite <$> strOption mempty) mempty))

bundleID :: Parser BundleID
bundleID = argument (readParser parseBundleID) mempty

word8 :: Parser Word8
word8 = argument (readParser parseWord8) mempty

word16 :: Parser Word16
word16 = argument (readParser parseWord16) mempty

bool :: Parser Bool
bool = argument (readParser parseBool) mempty

rgb :: Parser RGB
rgb = RGB <$> word8 <*> word8 <*> word8

allocSize :: Parser Int
allocSize = argument (readParser parseAllocSize) mempty

fshandle :: Parser FSHandle
fshandle = argument (readParser parseFSHandle) mempty
