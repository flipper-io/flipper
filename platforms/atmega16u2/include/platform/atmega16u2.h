#ifndef __platform_h__
#define __platform_h__

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

/* These macros are defined in platform specific header files. */
#define enable_interrupts() sei()
#define disable_interrupts() cli()

#define baudrate(baudrate) ((F_CPU / 8 / baudrate - 1) / 2)

#define delay_ms(ms) _delay_ms(ms)
#define delay_seconds(sec) delay_ms(sec * 1000)

#define crc_attributes PROGMEM
#define crc_access(dereference) pgm_read_word(&dereference)

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
