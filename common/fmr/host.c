#define __private_include__

#include <usb/usb.h>

#include <fmr/fmr.h>

const struct _target host = {
	
	host_configure,
	
	host_invoke,
	
	host_push,
	
	host_pull
	
};

void host_configure(const struct _bus *bus) {
	
	/* ~ Configure the host's communication protocol. ~ */
	
	((struct _target *)(&host)) -> bus = &usb;
	
}

uint32_t host_invoke(uint8_t object, uint8_t index, uint8_t argc, ...) {
		
	/* ~ Construct a va_list to access variadic arguments. ~ */
	
	va_list argv;
	
	/* ~ Initialize the va_list that we created above. ~ */
	
	va_start(argv, argc);
	
	/* ~ Invoke the function on the selected target. ~ */
	
	return target_invoke(&host, object, index, argc, &argv);
	
}

uint32_t host_push(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...) {
	
	return 0;
	
}

void host_pull(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...) {
	
	
	
}