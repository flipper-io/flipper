/* the libflipper configuration header for the atmegau2 platform
   the chip model is atmel atmega16u2 */

#ifndef __lf_config_h__
#define __lf_config_h__

/* define the flags needed to properly configure avr-libc for this chip */
#define ARCH ARCH_AVR8
#define __AVR_ATmega32U2__

/* the chip has a 16MHz oscillator (https://github.com/flipper-io/flipper/wiki/Flipper-Carbon) */
#define F_CPU 16000000UL

/* define the flags needed to make libflipper work on this chip */
#define ATMEGAU2

/* don't enable taking backtraces of libflipper on this chip */
#define LF_DISABLE_DEBUG

/* the atmegau2 doesn't have much memory, so we can't afford to ship all
   of the error strings in the copy of libflipper that goes on the chip */
#define LF_CONFIG_OMIT_ERRORS

#endif
