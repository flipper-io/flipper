#ifndef __platform_h__
#define __platform_h__

#include "atmel.h"

/* Include Flipper Message Runtime specific definitions for this platform. */
#include "fmr.h"

#define SS AT91C_PA31_NPCS1
#define FLASH_CS_PIN 31

#define baudrate(baudrate) (F_CPU / 16 / baudrate)

#define crc_attributes
#define crc_access(dereference) dereference

extern void _delay_ms(unsigned long time);
#define delay_ms(ms) _delay_ms(ms)
#define delay_seconds(sec) delay_ms(sec * 1000)

char serial[64];
#define serprintf(...) usart1.push(serial, sprintf(serial, __VA_ARGS__));

#endif