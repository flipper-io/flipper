#define __private_include__
#include <flipper/error.h>
#include <flipper/flipper.h>

#define KNRM  "\x1B[0m"
#define KRED  "\x1B[31m"
#define KBLU  "\x1B[34m"
#define KYEL  "\x1B[33m"

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
/* When set, this flag allows errors to cause system side effects. */
#ifdef __enable_error_side_effects__
	/* If the selected device is configured to cause side effects, do so. */
	if (flipper.device -> errors_generate_side_effects) {
		/* Construct a va_list to access variadic arguments. */
		va_list argv;
		/* Initialize the va_list that we created above. */
		va_start(argv, format);
raise:
		/* Print the exception if a message is provided. */
		if (format) {
			fprintf(stderr, KYEL "\nThe Flipper runtime encountered the following error:\n  " KNRM "↳ " KRED "'");
			vfprintf(stderr, format, argv);
			fprintf(stderr, "'\n");
		}
		/* Expose the error message strings. */
		char *message[] = { LF_ERROR_MESSAGE_STRINGS };
		/* Print the error code. */
		fprintf(stderr, KNRM "Error code (%i): '" KBLU "%s" KNRM "'\n\n", _error, message[_error]);
		/* Release the va_list. */
		va_end(argv);
		/* Exit. */
		exit(EXIT_FAILURE);
	}
#else
raise:
	return;
#endif
}

lf_error_t error_get(void) {
	/* If no device is selected, raise an error. */
	if (!flipper.device) {
		error_raise(E_NO_DEVICE, NULL);
		return lf_error;
	}
	return flipper.device -> error;
}

void error_clear(void) {
	/* If no device is selected, raise an error. */
	if (!flipper.device) {
		error_raise(E_NO_DEVICE, NULL);
		return;
	}
	flipper.device -> error = E_OK;
}

void error_resume(void) {
	/* If no device is selected, raise an error. */
	if (!flipper.device) {
		error_raise(E_NO_DEVICE, NULL);
		return;
	}
	/* Change the configuration. */
	flipper.device -> errors_generate_side_effects = true;
}

void error_pause(void) {
	/* If no device is selected, raise an error. */
	if (!flipper.device) {
		error_raise(E_NO_DEVICE, NULL);
		return;
	}
	/* Change the configuration. */
	flipper.device -> errors_generate_side_effects = false;
}
