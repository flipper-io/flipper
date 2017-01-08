/* Flipper: Carbon Edition board header file. */
#include <flipper/libflipper.h>

/* Include the header files for all of the standard modules exposed by the toolbox. */
#include <flipper/carbon/modules/adc.h>
#include <flipper/carbon/modules/button.h>
#include <flipper/carbon/modules/cpu.h>
#include <flipper/carbon/modules/dac.h>
#include <flipper/carbon/modules/fld.h>
#include <flipper/carbon/modules/fs.h>
#include <flipper/carbon/modules/gpio.h>
#include <flipper/carbon/modules/i2c.h>
#include <flipper/carbon/modules/led.h>
#include <flipper/carbon/modules/pwm.h>
#include <flipper/carbon/modules/rtc.h>
#include <flipper/carbon/modules/spi.h>
#include <flipper/carbon/modules/swd.h>
#include <flipper/carbon/modules/task.h>
#include <flipper/carbon/modules/temp.h>
#include <flipper/carbon/modules/timer.h>
#include <flipper/carbon/modules/uart0.h>
#include <flipper/carbon/modules/usart.h>
#include <flipper/carbon/modules/usb.h>
#include <flipper/carbon/modules/wdt.h>

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
