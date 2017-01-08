#ifndef __lf_modules_h__
#define __lf_modules_h__

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
};
/* NOTE: The identifiers in this enumeration must match the order of the module addresses stored in the 'fmr_modules' array. */

#endif
#endif
