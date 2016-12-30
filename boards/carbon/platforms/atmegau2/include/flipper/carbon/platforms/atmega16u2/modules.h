#ifndef __lf_modules_h__
#define __lf_modules_h__

/* NOTE: This header file should only be included once by modules.c for each platform. */

/* Include the header files for all of the standard modules exposed by the toolbox. */
#define __private_include__
#include <flipper/carbon/button.h>
#include <flipper/carbon/cpu.h>
#include <flipper/carbon/error.h>
#include <flipper/fmr.h>
#include <flipper/carbon/fs.h>
#include <flipper/carbon/led.h>
#include <flipper/carbon/spi.h>
#include <flipper/carbon/uart0.h>
#include <flipper/carbon/wdt.h>

/* Create an enumeraion defining all of the precomputed identifiers needed for the standard modules. */
enum {
	#define __use_button__
	_button_id,
	#define __use_cpu__
	_cpu_id,
	#define __use_fmr__
	_fmr_id,
	#define __use_fs__
	_fs_id,
	#define __use_led__
	_led_id,
	#define __use_uart0__
	_uart0_id,
	#define __use_wdt__
	_wdt_id,
};
/* NOTE: The identifiers in this enumeration must match the order of the module addresses stored in the 'fmr_modules' array. */

#endif
