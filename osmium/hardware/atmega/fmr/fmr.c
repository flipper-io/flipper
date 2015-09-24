#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include <platform/atmega.h>

fmr_packet fmrpacket;

const void * const objects[] PROGMEM = { &button, &flash, &host, &self, &device, &fs, &i2c, &io, &led, &pwm, &spi, &timer, &usart, &usart, &usart, &usb, &wifi };