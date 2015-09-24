#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

fmr_packet fmrpacket;

const void * const objects[] = { &button, &flash, &host, &self, &device, &fs, &i2c, &io, &led, &pwm, &spi, &timer, &usart, &usart1, &dbgu, &usb };