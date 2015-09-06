#define __private_include__

#include <drivers/usb.h>

#include <fmr/fmr.h>

const struct _target device = {
	
	device_configure,
	
	device_invoke,
	
	device_push,
	
	device_pull
	
};

void device_configure(void) {
	
	/* ~ Configure USB as the device's communication protocol. ~ */
	
	((struct _target *)(&device)) -> bus = &usb;
	
}

uint32_t device_invoke(uint8_t module, uint8_t index, uint8_t argc, ...) {
	
	/* ~ Construct a va_list to access varidic arguments. ~ */
	
	va_list argv;
	
	/* ~ Initialize the va_list that we created above. ~ */
	
	va_start(argv, argc);
	
	/* ~ Invoke the function on the selected target. ~ */
	
	return target_invoke(&device, module, index, argc, &argv);
	
}

uint32_t device_push(uint8_t module, uint8_t index, uint8_t argc, void *source, uint32_t length, ...) {
	
	return 0;
	
}

void device_pull(uint8_t module, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...) {
	
	
	
}