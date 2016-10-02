module Flipper.Console.Options where

import Control.Applicative

import Data.Foldable
import Data.Word

import Flipper
import Flipper.GPIO (DigitalPin(..), AnalogPin(..), Direction(..))
import Flipper.LED

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
install = info (Install <$> moduleID <*> installP) installI
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

format :: ParserInfo ConsoleAction
format = info (pure Format) formatI
    where formatI = mconcat [ fullDesc
                            ]

call :: Parser Call
call = subparser $ mconcat [ command "button" (ButtonCall <$> button)
                           , command "fs" (FSCall <$> fs)
                           , command "gpio" (GPIOCall <$> gpio)
                           , command "led" (LEDCall <$> led)
                           , command "spi" (SPICall <$> spi)
                           , command "uart" (UARTCall <$> uart)
                           ]

button :: ParserInfo ButtonAction
button = info buttonRead buttonI
    where buttonI = mconcat [ fullDesc
                            ]

fs :: ParserInfo FSAction
fs = info fsP fsI
    where fsP = asum [ fsCreateFromString
--                     , fsCreateFromFile
                     , fsRemove
                     , fsRename
                     ]
          fsI = mconcat [ fullDesc
                        ]

gpio :: ParserInfo GPIOAction
gpio = info (gpioDirection <|> gpioRead <|> gpioWrite) ioI
    where ioI = mconcat  [ fullDesc
                         ]

led :: ParserInfo LEDAction
led = info ledRGB ledI
    where ledI = mconcat [ fullDesc
                         ]

spi :: ParserInfo SPIAction
spi = info spiP spiI
    where spiI = mconcat [ fullDesc
                         ]
          spiP = asum [ spiEnable
                      , spiDisable
                      , spiRead
                      , spiWriteFromString
--                      , spiWriteFromFile
                      ]

uart :: ParserInfo UARTAction
uart = info uartP uartI
    where uartI = mconcat [ fullDesc
                          ]
          uartP = asum [ uartEnable
                       , uartDisable
                       , uartRead
                       , uartWriteFromString
--                       , uartWriteFromFile
                       ]

buttonRead :: Parser ButtonAction
buttonRead = subparser (command "read" (info (pure ButtonRead) mempty))

fsCreateFromString :: Parser FSAction
fsCreateFromString = subparser (command "create" (info (FSCreateFromString <$> strOption mempty <*> strOption mempty) mempty))

fsRemove :: Parser FSAction
fsRemove = subparser (command "remove" (info (FSRemove <$> strOption mempty) mempty))

fsRename :: Parser FSAction
fsRename = subparser (command "rename" (info (FSRename <$> strOption mempty <*> strOption mempty) mempty))

gpioDirection :: Parser GPIOAction
gpioDirection = subparser (command "direction" (info (digitalDirection <|> analogDirection) mempty))

digitalPin :: Parser DigitalPin
digitalPin = argument (readParser parseDigitalPin) mempty

analogPin :: Parser AnalogPin
analogPin = argument (readParser parseAnalogPin) mempty

direction :: Parser Direction
direction = argument (readParser parseDirection) mempty

digitalDirection :: Parser GPIOAction
digitalDirection = GPIODigitalDirection <$> digitalPin <*> direction

analogDirection :: Parser GPIOAction
analogDirection = GPIOAnalogDirection <$> analogPin <*> direction

gpioRead :: Parser GPIOAction
gpioRead = subparser (command "read" (info (digitalRead <|> analogRead) mempty))

digitalRead :: Parser GPIOAction
digitalRead = GPIODigitalRead <$> digitalPin

analogRead :: Parser GPIOAction
analogRead = GPIOAnalogRead <$> analogPin

gpioWrite :: Parser GPIOAction
gpioWrite = subparser (command "write" (info (digitalWrite <|> analogWrite) mempty))

digitalWrite :: Parser GPIOAction
digitalWrite = GPIODigitalWrite <$> digitalPin <*> bool

analogWrite :: Parser GPIOAction
analogWrite = GPIOAnalogWrite <$> analogPin <*> word16

ledRGB :: Parser LEDAction
ledRGB = subparser (command "rgb" (info (LEDSetRGB <$> rgb) mempty))

spiEnable :: Parser SPIAction
spiEnable = subparser (command "enable" (info (pure SPIEnable) mempty))

spiDisable :: Parser SPIAction
spiDisable = subparser (command "disable" (info (pure SPIDisable) mempty))

spiRead :: Parser SPIAction
spiRead = subparser (command "read" (info (pure SPIRead) mempty))

spiWriteFromString :: Parser SPIAction
spiWriteFromString = subparser (command "write" (info (SPIWriteFromString <$> strOption mempty) mempty))

uartEnable :: Parser UARTAction
uartEnable = subparser (command "enable" (info (pure UARTEnable) mempty))

uartDisable :: Parser UARTAction
uartDisable = subparser (command "disable" (info (pure UARTDisable) mempty))

uartRead :: Parser UARTAction
uartRead = subparser (command "read" (info (pure UARTRead) mempty))

uartWriteFromString :: Parser UARTAction
uartWriteFromString = subparser (command "write" (info (UARTWriteFromString <$> strOption mempty) mempty))

moduleID :: Parser ModuleID
moduleID = argument (readParser parseModuleID) mempty

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
