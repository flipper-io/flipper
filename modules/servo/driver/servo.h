#ifndef servo_h

#define servo_h

#include <flipper/types.h>

#include <fmr/fmr.h>

/* Declare the virtual interface for the servo driver. */

extern const struct _servo {
	
	void (* configure)(void);
	
	void (* attach)(uint8_t pin);
	
	void (* rotate)(uint32_t position);
	
} servo;

/* Define the private definitions of the driver functions. */

#ifdef __private_include__

/* Declare the enum overlay for this driver. */

enum { _servo_configure, _servo_attach, _servo_rotate };

/* Configures the necessary IO for the interface. */

extern void servo_configure(void);

extern void servo_attach(uint8_t pin);

extern void servo_rotate(uint32_t position);

#endif

#endif