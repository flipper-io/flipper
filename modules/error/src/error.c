#define __private_include__
#include <flipper/error.h>
#include <flipper/flipper.h>

#define KNRM  "\x1B[0m"
#define KRED  "\x1B[31m"
#define KBLU  "\x1B[34m"
#define KYEL  "\x1B[33m"

char *error_messages[] = { "no error", "malloc failure", "null pointer", "overflow", "no target device", "device not attached", "device already attached", "file already exists", "file does not exist", "packet overflow", "endpoint error", "libusb error", "transfer error", "socket error", "no module found" };

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
			fprintf(stderr, KYEL "\nThe Flipper runtime encountered the following error:\n  " KRED "'");
			vfprintf(stderr, format, argv);
			fprintf(stderr, "'\n");
		}
		/* Print the error code. */
		fprintf(stderr, KNRM "Error code (%i): '" KBLU "%s" KNRM "'\n\n", _error, error_messages[_error]);
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
