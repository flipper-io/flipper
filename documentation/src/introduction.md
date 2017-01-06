## Introduction

Flipper is an embedded development platform designed from the ground up to
provide seamless abstraction between the application layer and common hardware
peripherals such as GPIO, I2C, SPI, UART, and more. In other words, Flipper
makes it easy to control **simple hardware** from **any programming language**
on **any platform** in **real-time**.

Flipper aims to simplify embedded systems development by allowing interactive
debugging, eliminating the compile-upload-debug paradigm experienced by many
systems programmers. Flipper hardware works out of the box; built in "standard"
modules enable the control of Flipper hardware from any platform with a supporting
**Flipper Toolbox** implementation.

### Flipper Toolbox

The Flipper Toolbox, or toolbox for short, is a repository of software
components that work together to enable dynamic control of Flipper hardware.
The toolbox is comprised of language bindings and platform support
libraries that extend the functionality of Flipper hardware to a vairety of
programming languages and operating environments.

The standard modules, and the hardware they control are described in the table
below.

|||
|-|-|
| <center>[**adc**](modules-adc.html)</center> | <left>Read analog values from supported IO pins.</left> |
| <center>[**button**](modules-button.html)</center> | <left>Get the pressed state of the pushbutton as a boolean.</left> |
| <center>[**cpu**](modules-cpu.html)</center> | <left>Reset, power cycle, power off the primary ARM CPU.</left> |
| <center>[**dac**](modules-dac.html)</center> | <left>Write analog values to supported IO pins.</left> |
| <center>[**fs**](modules-fs.html)</center> | <left>Create, rename, delete, upload/download files to/from the device.</left> |
| <center>[**gpio**](modules-gpio.html)</center> | <left>Configure GPIO pins, write values to outputs, read boolean values from inputs.</left> |
| <center>[**i2c**](modules-i2c.html)</center> | <left>Manage I2C/TWI transfers using SDA and SCK.</left> |
| <center>[**led**](modules-led.html)</center> | <left>Write a red, green, and blue value to the onboard RGB LED.</left> |
| <center>[**pwm**](modules-pwm.html)</center> | <left>Output specific PWM frequencies to supported IO pins.</left> |
| <center>[**rtc**](modules-rtc.html)</center> | <left>Get the date and time from the Gregorian or Persian calendar.</left> |
| <center>[**spi**](modules-spi.html)</center> | <left>Manage transfers to SPI devices using MOSI MISO SCK and SS.</left> |
| <center>[**swd**](modules-swd.html)</center> | <left>Control the active debugging session; step, continue, break, etc.</left> |
| <center>[**temp**](modules-temp.html)</center> | <left>Get the ambient temperature in Farenheit or Celcius.</left> |
| <center>[**timer**](modules-timer.html)</center> | <left>Register function callbacks after a certain duration of time.</left> |
| <center>[**usart**](modules-usart.html)</center> | <left>Manage transfers to USART devices using TX and RX.</left> |
| <center>[**usb**](modules-usb.html)</center> | <left>Act as a USB device such as a keyboard and mouse using D+ and D-.</left> |
| <center>[**wdt**](modules-wdt.html)</center> | <left>Configure, enable, pet, or disable the internal watchdog timer.</left> |
