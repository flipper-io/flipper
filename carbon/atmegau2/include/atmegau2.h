/* Flipper Platform Support Header for the Atmel ATMEGAU2 */

/* NOTE: Only one target platform can be included at a time. */

/* Guard whether or not a target platform has already been defined. */
#ifndef __atmegau2__
#define __atmegau2__

#ifdef __AVR__

/* Include the avr-libc standard library headers. */
#include <avr/common.h>
#include <util/delay.h>
#include <avr/eeprom.h>
#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/pgmspace.h>
#include <avr/power.h>
#include <avr/sleep.h>
#include <avr/wdt.h>

#undef lf_debug
#define lf_debug(fmt, ...) printf_P(PSTR(fmt"\n"), ##__VA_ARGS__)

#undef lf_assert
#define lf_assert(cond, err, fmt, ...) if (!(cond)) { _lf_assert(err, __func__, __LINE__, PSTR(fmt), ##__VA_ARGS__); goto fail; }

#endif

/* Descriptor information for USB. */
#define USB_MANUFACTURER        L"flipper.io"
#define USB_PRODUCT             L"Flipper: Carbon"
#define USB_PRODUCT_ID          0x0480

/* NOTE: Summing the size parameters of each endpoints below should be less than or equal to 160. */
#define USB_IN_MASK             0x80

#define INTERRUPT_IN_ENDPOINT	(0x01 | USB_IN_MASK)
#define INTERRUPT_IN_SIZE		16
#define INTERRUPT_OUT_ENDPOINT	0x02
#define INTERRUPT_OUT_SIZE		16

#define BULK_IN_ENDPOINT		(0x01 | USB_IN_MASK)
#define BULK_IN_SIZE			64
#define BULK_OUT_ENDPOINT		0x02
#define BULK_OUT_SIZE			64

#define CONTROL_INTERFACE       0

#define DEBUG_INTERFACE			1
#define DEBUG_IN_ENDPOINT		(0x03 | USB_IN_MASK)
#define DEBUG_IN_SIZE			32

/* NOTE: Move these to platform. */
#define DFU_BAUD 115200
#define FMR_BAUD 1000000

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

#define FMR_DDR DDRC
#define FMR_IN PINC
#define FMR_PIN 0x07

#endif
