module Flipper.Console.Error where

import Data.Monoid

import Flipper.Error
import Flipper.Console.Action

consoleError :: Maybe ConsoleAction -> FlipperError -> String
consoleError c e = (commandCtx c) <> "\n" <> (errorMsg e)

commandCtx :: Maybe ConsoleAction -> String
commandCtx Nothing                = "No command context."
commandCtx (Just (Flash fp))      = "Flashing file " <> fp
commandCtx (Just (Install md fp)) = mconcat [ "Installing module "
                                            , show md
                                            , " from file "
                                            , fp
                                            ]
commandCtx (Just (Launch p))      = "Launching program " <> p
commandCtx (Just Reset)           = "Resetting device."
commandCtx (Just Suspend)         = "Suspending device."
commandCtx (Just Format)          = "Formatting device flash."
commandCtx (Just (ConsoleCall c)) = callCtx c

callCtx :: Call -> String
callCtx (ADCCall a)    = adcCtx a
callCtx (ButtonCall b) = buttonCtx b
callCtx (DACCall d)    = dacCtx d
callCtx (FSCall f)     = fsCtx f
callCtx (GPIOCall g)   = gpioCtx g
callCtx (I2CCall i)    = i2cCtx i
callCtx (LEDCall l)    = ledCtx l
callCtx (PWMCall p)    = pwmCtx p
callCtx (RTCCall r)    = rtcCtx r
callCtx (SPICall s)    = spiCtx s
callCtx (SWDCall s)    = swdCtx s
callCtx (TempCall t)   = tempCtx t
callCtx (TimerCall t)  = timerCtx t
callCtx (UART0Call u)  = uart0Ctx u
callCtx (USARTCall u)  = usartCtx u
callCtx (USBCall u)    = usbCtx u
callCtx (WDTCall w)    = wdtCtx w

adcCtx :: ADCAction -> String
adcCtx ADCConfigure = "Configuring ADC."

buttonCtx :: ButtonAction -> String
buttonCtx ButtonConfigure = "Configuring button."
buttonCtx ButtonRead      = "Reading button state."

dacCtx :: DACAction -> String
dacCtx DACConfigure = "Configuring DAC."

fsCtx :: FSAction -> String
fsCtx FSConfigure      = "Configure file system."
fsCtx (FSCreate fn)    = "Creating file " <> fn
fsCtx (FSDelete fn)    = "Deleting file " <> fn
fsCtx (FSSize fn)      = "Querying size of file " <> fn
fsCtx (FSOpen fn o)    = mconcat [ "Opening file "
                                 , fn
                                 , " at offset "
                                 , show o
                                 ]
fsCtx (FSPushString p) = "Pushing payload " <> p
fsCtx FSPullString     = "Pulling payload."
fsCtx FSClose          = "Closing open file."

gpioCtx :: GPIOAction -> String
gpioCtx GPIOConfigure              = "Configuring GPIO."
gpioCtx (GPIODigitalDirection p d) = mconcat [ "Setting direction of pin "
                                             , show p
                                             , " to "
                                             , show d
                                             ]
gpioCtx (GPIODigitalRead p)        = "Reading pin " <> show p
gpioCtx (GPIODigitalWrite p v)     = mconcat [ "Setting value of pin "
                                             , show p
                                             , " to "
                                             , show v
                                             ]
gpioCtx (GPIOAnalogDirection p d)  = mconcat [ "Setting direction of pin "
                                             , show p
                                             , " to "
                                             , show d
                                             ]
gpioCtx (GPIOAnalogRead p)         = "Reading pin " <> show p
gpioCtx (GPIOAnalogWrite p v)      = mconcat [ "Setting value of pin "
                                             , show p
                                             , " to "
                                             , show v
                                             ]

i2cCtx :: I2CAction -> String
i2cCtx I2CConfigure = "Configuring I2C."

ledCtx :: LEDAction -> String
ledCtx LEDConfigure  = "Configuring LED."
ledCtx (LEDSetRGB c) = "Setting LED color to " <> show c

pwmCtx :: PWMAction -> String
pwmCtx PWMConfigure = "Configuring PWM."

rtcCtx :: RTCAction -> String
rtcCtx RTCConfigure = "Configuring RTC."

spiCtx :: SPIAction -> String
spiCtx SPIConfigure           = "Configuring SPI bus."
spiCtx SPIEnable              = "Enabling SPI bus."
spiCtx SPIDisable             = "Disabling SPI bus."
spiCtx SPIRead                = "Reading from SPI bus."
spiCtx (SPIWriteFromString p) = "Writing to SPI bus: " <> p
spiCtx (SPIWriteFromFile fp)  = mconcat [ "Writing file"
                                        , fp
                                        , " to SPI bus."
                                        ]

swdCtx :: SWDAction -> String
swdCtx SWDConfigure = "Configuring SWD."

tempCtx :: TempAction -> String
tempCtx TempConfigure = "Configuring thermal hardware."

timerCtx :: TimerAction -> String
timerCtx TimerConfigure = "Configuring timer."

uart0Ctx :: UART0Action -> String
uart0Ctx UART0Configure           = "Configuring UART0 bus."
uart0Ctx UART0Enable              = "Enabling UART0 bus."
uart0Ctx UART0Disable             = "Disabling UART0 bus."
uart0Ctx UART0Read                = "Reading from UART0 bus."
uart0Ctx (UART0WriteFromString p) = "Writing to UART0 bus: " <> p
uart0Ctx (UART0WriteFromFile fp)  = mconcat [ "Writing file"
                                            , fp
                                            , " to UART0 bus."
                                            ]

usartCtx :: USARTAction -> String
usartCtx USARTConfigure           = "Configuring USART bus."
usartCtx USARTEnable              = "Enabling USART bus."
usartCtx USARTDisable             = "Disabling USART bus."
usartCtx USARTRead                = "Reading from USART bus."
usartCtx (USARTWriteFromString p) = "Writing to USART bus: " <> p
usartCtx (USARTWriteFromFile fp)  = mconcat [ "Writing file"
                                            , fp
                                            , " to USART bus."
                                            ]

usbCtx :: USBAction -> String
usbCtx USBConfigure = "Configuring USB."

wdtCtx :: WDTAction -> String
wdtCtx WDTConfigure = "Configuring WDT."

errorMsg :: FlipperError -> String
errorMsg OK                 = "OK, no error."
errorMsg MemAllocFailed     = "Memory allocation failed."
errorMsg Null               = "Null pointer encountered."
errorMsg Overflow           = "Overflow."
errorMsg NoDevice           = "No device present."
errorMsg NotAttached        = "Device is not attached."
errorMsg AlreadyAttached    = "Device is already attached."
errorMsg FileAlreadyExists  = "File already exists."
errorMsg FileNotFound       = "File not found."
errorMsg FMRPacketOverflow  = "FMR packet overflow."
errorMsg FMRError           = "FMR error."
errorMsg Endpoint           = "Endpoint error."
errorMsg USB                = "libusb error."
errorMsg Communication      = "Device communication error."
errorMsg Socket             = "Socket error."
errorMsg Module             = "Module not found."
errorMsg Resolution         = "Module dispatch resolution error."
errorMsg NoString           = "String not found."
errorMsg Checksum           = "Checksum error."
errorMsg Name               = "Name not found."
errorMsg Configuration      = "Cofiguration read error."
errorMsg Acknowledge        = "Device failed to acknowledge."
errorMsg Unknown            = "Unknown error."
