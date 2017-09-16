#ifndef __atmega16u2_h__
#define __atmega16u2_h__

/* Include the avr-libc standard library headers. */
#ifdef __ATMEGAU2__
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

/* Create an enumeraion defining all of the precomputed identifiers needed for the standard modules. */
enum {
	#define __use_button__
	_button_id,
	#define __use_fs__
	_fs_id,
	#define __use_gpio__
	_gpio_id,
	#define __use_led__
	_led_id,
	#define __use_uart0__
	_uart0_id,
	#define __use_wdt__
	_wdt_id,
};
/* NOTE: The identifiers in this enumeration must match the order of the module addresses stored in the 'fmr_modules' array. */

/* These macros are defined in platform specific header files. */
#define enable_interrupts() sei()
#define disable_interrupts() cli()

#define delay_ms(ms) _delay_ms(ms)

/* Pin definition macros for F3 hardware. */
#define FLASH_CS_DDR            DDRB
#define FLASH_CS_PORT           PORTB
#define FLASH_CS_PIN            0x00

#define FLASH_WP_DDR            DDRC
#define FLASH_WP_PORT           PORTC
#define FLASH_WP_PIN            0x02

#define FLASH_RESET_DDR         DDRD
#define FLASH_RESET_PORT        PORTD
#define FLASH_RESET_PIN         0x00

#define LED_DDR                 DDRC
#define LED_PORT                PORTC
#define LED_DI                  0x05

#define SPI_DDR                 DDRB
#define SS                      0x00
#define SCK                     0x01
#define MOSI                    0x02
#define MISO                    0x03

#define BUTTON_DDR              DDRC
#define BUTTON_IN               PINC
#define BUTTON_PIN              0x06

#define SAM_POWER_DDR           DDRD
#define SAM_POWER_PORT          PORTD
#define SAM_POWER_PIN           0x01

#define SAM_RESET_DDR           DDRD
#define SAM_RESET_PORT          PORTD
#define SAM_RESET_PIN           0x05

#define SAM_TEST_DDR            DDRD
#define SAM_TEST_PORT           PORTD
#define SAM_TEST_PIN            0x04

#define SAM_ERASE_DDR           DDRD
#define SAM_ERASE_PORT          PORTD
#define SAM_ERASE_PIN           0x06

#endif
