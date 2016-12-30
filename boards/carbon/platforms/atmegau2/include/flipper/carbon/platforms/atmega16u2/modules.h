#ifndef __lf_modules_h__
#define __lf_modules_h__

/* Include the header files for all of the standard modules exposed by the toolbox. */
#include <flipper/carbon/modules/button.h>
#include <flipper/carbon/modules/cpu.h>
#include <flipper/carbon/modules/fs.h>
#include <flipper/carbon/modules/led.h>
#include <flipper/carbon/modules/spi.h>
#include <flipper/carbon/modules/uart0.h>
#include <flipper/carbon/modules/wdt.h>

/* NOTE: This include should only be here until FMR is removed as a module. */
#include <flipper/fmr.h>

#ifdef __private_include__

/* Create an enumeraion defining all of the precomputed identifiers needed for the standard modules. */
enum {
	#define __use_button__
	_button_id,
	#define __use_cpu__
	_cpu_id,
	#define __use_fs__
	_fs_id,
	#define __use_led__
	_led_id,
	#define __use_uart0__
	_uart0_id,
	#define __use_wdt__
	_wdt_id,
	/* NOTE: Remove FMR as a module. */
	#define __use_fmr__
	_fmr_id,
};
/* NOTE: The identifiers in this enumeration must match the order of the module addresses stored in the 'fmr_modules' array. */

#endif
#endif
