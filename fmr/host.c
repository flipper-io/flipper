#define __private_include__
#include <fmr/fmr.h>
#include <platform.h>

struct _target host = {
	
	host_configure,
	host_call,
	host_invoke,
	host_push,
	host_pull
	
};

void host_configure(const struct _bus *bus) {
	
	/* ~ Configure the host's communication protocol. ~ */
	((struct _target *)(&host)) -> bus = bus;
	((struct _target *)(&host)) -> id = _host;
	
}

uint32_t host_call(void) {
	
	return 0;
	
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
	
	/* ~ Construct a va_list to access variadic arguments. ~ */
	va_list argv;
	
	/* ~ Initialize the va_list that we created above. ~ */
	va_start(argv, length);
	
	/* ~ Invoke the function on the selected target. ~ */
	return target_push(&host, object, index, argc, source, length, &argv);
	
}

uint32_t host_pull(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...) {
	
	/* ~ Construct a va_list to access variadic arguments. ~ */
	va_list argv;
	
	/* ~ Initialize the va_list that we created above. ~ */
	va_start(argv, length);
	
	/* ~ Invoke the function on the selected target. ~ */
	return target_pull(&host, object, index, argc, destination, length, &argv);
	
}
