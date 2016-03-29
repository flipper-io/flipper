#define __private_include__
#include <flipper/fmr.h>
#include <flipper/platform/platform.h>

struct _target device = {
	device_configure,
	device_call,
	device_invoke,
	device_push,
	device_pull
};

void device_configure(const struct _bus *bus) {

	/* ~ Configure the device's communication protocol. ~ */
	device.bus = bus;
	device.id = _device;

}

uint32_t device_call(void) {

	/* ~ Send the packet to the device. ~ */
	device.bus -> push(&fmrpacket, fmrpacket.header.length);

	/* ~ Return the result to the caller. ~ */
	return fmr_obtain_response(&device);

}

uint32_t device_invoke(uint8_t object, uint8_t index, uint8_t argc, ...) {

	/* ~ Construct a va_list to access variadic arguments. ~ */
	va_list argv;

	/* ~ Initialize the va_list that we created above. ~ */
	va_start(argv, argc);

	/* ~ Invoke the function on the selected target. ~ */
	return target_invoke(&device, object, index, argc, &argv);

}

uint32_t device_push(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...) {

	/* ~ Construct a va_list to access variadic arguments. ~ */
	va_list argv;

	/* ~ Initialize the va_list that we created above. ~ */
	va_start(argv, length);

	/* ~ Invoke the function on the selected target. ~ */
	return target_push(&device, object, index, argc, source, length, &argv);

}

uint32_t device_pull(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...) {

	/* ~ Construct a va_list to access variadic arguments. ~ */
	va_list argv;

	/* ~ Initialize the va_list that we created above. ~ */
	va_start(argv, length);

	/* ~ Invoke the function on the selected target. ~ */
	return target_pull(&device, object, index, argc, destination, length, &argv);

}
