#define __private_include__
#include <flipper/error.h>
#include <flipper/flipper.h>

#define KNRM  "\x1B[0m"
#define KRED  "\x1B[31m"
#define KBLU  "\x1B[34m"
#define KYEL  "\x1B[33m"

/* Expose the error message strings. */
char *messages[] = { LF_ERROR_MESSAGE_STRINGS };

void error_raise(lf_error_t error, const char *format, ...) {
	/* Create a local copy of the error code. */
	lf_error_t _error;
	/* If no device is selected, raise an error manually. */
	if (!flipper.device) {
		_error = error;
		goto raise;
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
raise:
		/* Initialize the va_list that we created above. */
		va_start(argv, format);
		if (_error > (sizeof(messages) / sizeof(char *))) {
			_error = E_STRING;
		}
		/* Print the exception if a message is provided. */
		if (format) {
			fprintf(stderr, KYEL "\nThe Flipper runtime encountered the following error:\n  " KNRM "↳ " KRED);
			if (_error == E_STRING) {
				fprintf(stderr, "An invalid error code (%i) was provided.\n", error);
			} else {
				vfprintf(stderr, format, argv);
				fprintf(stderr, "\n");
			}
		}
		/* Print the error code. */
		fprintf(stderr, KNRM "Error code (%i): '" KBLU "%s" KNRM "'\n\n", _error, messages[_error]);
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
	return lf_device() -> error;
}

void error_clear(void) {
	lf_device() -> error = E_OK;
}

void error_resume(void) {
	lf_device() -> errors_generate_side_effects = true;
}

void error_pause(void) {
	lf_device() -> errors_generate_side_effects = false;
}
