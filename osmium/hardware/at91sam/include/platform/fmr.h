#ifndef fmr_platform_h

#define fmr_platform_h

enum { _button, _flash, _host, _self, _device, _fs, _i2c, _io, _led, _pwm, _sam, _spi, _timer, _usart, _usart1, _dbgu, _usb, _wifi, _fdl, _bme280 };

#define fmr_access_array(object) objects[object]

#endif