/* Flipper: Carbon Edition board header file. */
#include <flipper/libflipper.h>

/* Include the header files for all of the standard modules exposed by the toolbox. */
#include <flipper/adc.h>
#include <flipper/button.h>
#include <flipper/dac.h>
#include <flipper/fld.h>
#include <flipper/fs.h>
#include <flipper/gpio.h>
#include <flipper/i2c.h>
#include <flipper/led.h>
#include <flipper/pwm.h>
#include <flipper/rtc.h>
#include <flipper/spi.h>
#include <flipper/swd.h>
#include <flipper/task.h>
#include <flipper/temp.h>
#include <flipper/timer.h>
#include <flipper/uart0.h>
#include <flipper/usart.h>
#include <flipper/usb.h>
#include <flipper/wdt.h>

/* Attaches to all carbon devices. */
int carbon_attach(void);
/* Attaches to a carbon device over the network. */
struct _lf_device *carbon_attach_hostname(char *hostname);
