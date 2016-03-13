#define __private_include__

#include "servo.h"

#ifdef __device_compilation__

__attribute__((section(".fdl")))

#endif

const struct _servo servo = {
	
	servo_configure,
	
	servo_attach,
	
	servo_rotate
	
};