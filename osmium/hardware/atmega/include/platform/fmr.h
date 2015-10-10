#ifndef fmr_platform_h

#define fmr_platform_h

#include <platform/atmega.h>

enum { _button, _flash, _host, _device, _self, _fs, _i2c, _io, _led, _pwm, _sam, _spi, _timer, _usart, _usart1, _dbgu, _usb, _wifi };

#define fmr_access_array(object) pgm_read_word(&objects[object])

#endif