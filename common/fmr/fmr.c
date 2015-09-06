#define __private_include__

#include <flipper/flipper.h>

#include <fmr/fmr.h>

uint8_t fmr_buffer[FLIPPER_PACKET_SIZE];

const void *system_modules[] = { &button, &flash, &self, &host, &device, &fs, &i2c, &io, &led, &pwm, &spi, &timer, &usart, &usb };

