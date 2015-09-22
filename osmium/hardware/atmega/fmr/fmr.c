#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include <platform/atmega.h>

uint8_t fmr_buffer[FLIPPER_PACKET_SIZE];

const void * const modules[] PROGMEM = { &button, &flash, &host, &self, &device, &fs, &i2c, &io, &led, &pwm, &spi, &timer, &usart, &usb };