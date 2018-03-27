/* Flipper Platform Support Header for the Atmel ATMEGAU2 */

/* NOTE: Only one target platform can be included at a time. */

/* Guard whether or not a target platform has already been defined. */
#ifndef __atmegau2__
#define __atmegau2__

/* Include the avr-libc standard library headers. */
#ifdef __AVR__
#include <avr/common.h>
#include <util/delay.h>
#include <avr/eeprom.h>
#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/pgmspace.h>
#include <avr/power.h>
#include <avr/sleep.h>
#include <avr/wdt.h>
#endif

/* Define the modules that this platform uses. */
#define __use_button__
#define __use_gpio__
#define __use_led__
#define __use_uart0__
#define __use_wdt__

/* Pin definition macros for F4 hardware. */

#define FLASH_CS_DDR DDRB
#define FLASH_CS_PORT PORTB
#define FLASH_CS_PIN 0x00

#define FLASH_WP_DDR DDRC
#define FLASH_WP_PORT PORTC
#define FLASH_WP_PIN 0x02

#define FLASH_RESET_DDR DDRD
#define FLASH_RESET_PORT PORTD
#define FLASH_RESET_PIN 0x00

#define LED_DDR DDRC
#define LED_PORT PORTC
#define LED_DI 0x05

#define SPI_DDR DDRB
#define SS 0x00
#define SCK 0x01
#define MOSI 0x02
#define MISO 0x03

#define BUTTON_DDR DDRC
#define BUTTON_IN PINC
#define BUTTON_PIN 0x06

#define SAM_POWER_DDR DDRD
#define SAM_POWER_PORT PORTD
#define SAM_POWER_PIN 0x01

#define SAM_RESET_DDR DDRD
#define SAM_RESET_PORT PORTD
#define SAM_RESET_PIN 0x04

#define SAM_TEST_DDR DDRD
#define SAM_TEST_PORT PORTD
#define SAM_TEST_PIN 0x05

#define SAM_ERASE_DDR DDRD
#define SAM_ERASE_PORT PORTD
#define SAM_ERASE_PIN 0x06

#define FSI_DDR DDRC
#define FSI_IN PINC
#define FSI_PIN 0x07

#endif
