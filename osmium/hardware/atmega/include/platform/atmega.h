#ifndef __atmega_h__

#define __atmega_h__

/* ~ Include platform specific definitions. ~ */

#include <platform/definitions.h>

/* ~ Include the avr-libc standard library headers. ~ */

#include <avr/common.h>

#include <util/delay.h>

#include <avr/eeprom.h>

#include <avr/interrupt.h>

#include <avr/io.h>

#include <avr/pgmspace.h>

#include <avr/power.h>

#include <avr/sleep.h>

#include <avr/wdt.h>


#if (defined(enable_interrupts) && defined(disable_interrupts))

#undef enable_interrupts

#undef disable_interrupts

#endif


#define enable_interrupts() sei()

#define disable_interrupts() cli()


#define baudrate(baudrate) ((F_CPU / 8 / baudrate - 1) / 2)


#define delay_ms(ms) _delay_ms(ms)

#define delay_seconds(sec) delay_ms(sec * 1000)


#define FLASH_CS_DDR		DDRB

#define FLASH_CS_PORT		PORTB

#define FLASH_CS_PIN		0x00


#define FLASH_WP_DDR		DDRB

#define FLASH_WP_PORT		PORTB

#define FLASH_WP_PIN		0x02


#define FLASH_RESET_DDR		DDRC

#define FLASH_RESET_PORT	PORTC

#define FLASH_RESET_PIN		0x04


#define LED_DDR				DDRB

#define LED_PORT			PORTB

#define LED_DI				0x06


#define SPI_PORT			DDRB

#define SS					0x00

#define SCK					0x01

#define MOSI				0x02

#define MISO				0x03


#define BUTTON_DDR			DDRD

#define BUTTON_IN			PIND

#define BUTTON_PIN			0x00


#define SAM_POWER_DDR		DDRD

#define SAM_POWER_PORT		PORTD

#define SAM_POWER_PIN		0x01


#define SAM_RESET_DDR		DDRD

#define SAM_RESET_PORT		PORTD

#define SAM_RESET_PIN		0x04


#define SAM_TEST_DDR		DDRD

#define SAM_TEST_PORT		PORTD

#define SAM_TEST_PIN		0x05


#define SAM_ERASE_DDR		DDRC

#define SAM_ERASE_PORT		PORTC

#define SAM_ERASE_PIN		0x05

#endif