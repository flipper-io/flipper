#define __private_include__
#include <flipper/error.h>
#include <flipper/flipper.h>

char *error_messages[] = { "No error.", "Malloc failure.", "Null pointer.", "Overflow.", "No target device.", "Device not attached.", "Device already attached.", "File already exists.", "File does not exist.", "FMR packet overflow.", "Endpoint error.", "Libusb error.", "Transfer error.", "Socket error.", "No module counterpart found." };

void error_raise(lf_error_t error, const char *format, ...) {
	/* Create a local copy of the error code. */
	lf_error_t _error;
	/* If no device is selected, raise an error manually. */
	if (!flipper.device) {
		_error = E_NO_DEVICE;
		goto raise;
		return;
	}
	/* Set the global error if the error being raised does not rely on the previous error state. */
	if (error != E_LAST) { flipper.device -> error = error; }
	/* Set the local copy of the error. */
	_error = flipper.device -> error;
	/* If the selected device is configured to cause side effects, do so. */
	if (flipper.device -> errors_cause_exit) {
		/* Construct a va_list to access variadic arguments. */
		va_list argv;
		/* Initialize the va_list that we created above. */
		va_start(argv, format);
raise:
		/* Print the exception if a message is provided. */
		if (format) {
			vfprintf(stderr, format, argv);
			fprintf(stderr, "\n\n");
		}
		/* Print the error code. */
		fprintf(stderr, "Error code (%i): %s\n\n", _error, error_messages[_error]);
		/* Release the va_list. */
		va_end(argv);
		/* Exit. */
		exit(EXIT_FAILURE);
	}
}

void error_resume(void) {
	/* If no device is selected, raise an error. */
	if (!flipper.device) {
		error_raise(E_NO_DEVICE, NULL);
		return;
	}
	/* Change the configuration. */
	flipper.device -> errors_cause_exit = true;
}

void error_pause(void) {
	/* If no device is selected, raise an error. */
	if (!flipper.device) {
		error_raise(E_NO_DEVICE, NULL);
		return;
	}
	/* Change the configuration. */
	flipper.device -> errors_cause_exit = false;
}
