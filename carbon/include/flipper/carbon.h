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

/* NOTE: Probably move this again. */
extern struct _lf_endpoint lf_bridge_ep;
/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
extern int lf_bridge_configure();
extern uint8_t lf_bridge_ready(struct _lf_endpoint *this);
extern void lf_bridge_put(struct _lf_endpoint *this, uint8_t byte);
extern uint8_t lf_bridge_get(struct _lf_endpoint *this);
extern int lf_bridge_push(struct _lf_endpoint *this, void *source, lf_size_t length);
extern int lf_bridge_pull(struct _lf_endpoint *this, void *destination, lf_size_t length);
extern int lf_bridge_destroy(struct _lf_endpoint *this);
