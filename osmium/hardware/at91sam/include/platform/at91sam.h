#ifndef __hardware_h__

#define __hardware_h__

#include <platform/atmel.h>

#define SS AT91C_PA31_NPCS1

#define baudrate(baudrate) (F_CPU / 16 / baudrate)


void _delay_ms(unsigned long time);

#define delay_ms(ms) _delay_ms(ms)

#define delay_seconds(sec) delay_ms(sec * 1000)


#define FLASH_CS_PIN 31


#endif