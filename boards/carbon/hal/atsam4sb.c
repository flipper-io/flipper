#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon.h>
#include <flipper/carbon/platforms/atsam4s16b/modules.h>

LF_MODULE(_adc, "adc", "Interfaces with the device's internal analog to digital converter.", _adc_id);
LF_MODULE(_button, "button", "Interacts with the onboard button.", _button_id);
LF_MODULE(_dac, "dac", "Provides access to the device's digital to analog converter.", _dac_id);
LF_MODULE(_fld, "fld", "Loads modules on the target device.", _fld_id);
LF_MODULE(_fs, "fs", "Provides access to the device's filesystem.", _fs_id);
LF_MODULE(_gpio, "gpio", "Interfaces with the device's general purpose input output pins.", _gpio_id);
LF_MODULE(_i2c, "i2c", "Interfaces with the device's I2C peripheral.", _i2c_id);
LF_MODULE(_led, "led", "Interacts with the built-in status LED.", _led_id);
LF_MODULE(_pwm, "pwm", "Interfaces with the device's pulse width modulation controller.", _pwm_id);
LF_MODULE(_rtc, "rtc", "Interfaces with the device's real time clock.", _rtc_id);
LF_MODULE(_spi, "spi", "Provides control over the device's SPI bus.", _spi_id);
LF_MODULE(_swd, "swd", "Interfaces with the device's single wire debug unit.", _swd_id);
LF_MODULE(_task, "task", "Pause, resume, stop the running application.", _task_id);
LF_MODULE(_temp, "temp", "Interfaces with the device's temperature sensor.", _temp_id);
LF_MODULE(_timer, "timer", "Interfaces with the device's timer peripheral.", _timer_id);
LF_MODULE(_usart, "usart", "Provides low level access to the device's UART bus.", _usart_id);
LF_MODULE(_usb, "usart", "Provides low level access to the device's USB bus.", _usb_id);
LF_MODULE(_wdt, "wdt", "Handles interaction with the internal watchdog timer.", _wdt_id);
