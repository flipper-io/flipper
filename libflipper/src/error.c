#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/error.h>

/* Expose the error message strings. */
char *lf_error_messages[] = { LF_ERROR_MESSAGE_STRINGS };
char last_error[256];

int lf_error_configure(void) {
	return lf_success;
}

void lf_error_raise(lf_error_t error, const char *format, ...) {
	lf_error_t _error = error;
	/* Record the observed error. */
	flipper.error_code = error;
#ifdef __enable_error_side_effects__
	if (error) {
		/* Construct a va_list to access variadic arguments. */
		va_list argv;
		/* Initialize the va_list that we created above. */
		va_start(argv, format);
		if (_error > (sizeof(lf_error_messages) / sizeof(char *))) {
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
		sprintf(last_error, KNRM "Error code (%i): '" KBLU "%s" KNRM "'\n\n", _error, lf_error_messages[_error]);
		/* Release the va_list. */
		va_end(argv);
		if (flipper.errors_cause_side_effects) {
			fprintf(stderr, "%s\n", last_error);
			/* Exit. */
			exit(EXIT_FAILURE);
		}
	}
#endif
	return;
}

char *lf_error_string(void) {
	return last_error;
}

lf_error_t error_get(void) {
	return flipper.error_code;
}

void lf_error_clear(void) {
	flipper.error_code = E_OK;
}

void lf_error_resume(void) {
	flipper.errors_cause_side_effects = 1;
}

void lf_error_pause(void) {
	flipper.errors_cause_side_effects = 0;
}
