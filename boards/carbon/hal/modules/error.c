#define __private_include__
#include <flipper/error.h>
#include <flipper/flipper.h>

/* Expose the error message strings. */
char *messages[] = { LF_ERROR_MESSAGE_STRINGS };

int error_configure(void) {
	return lf_success;
}

void error_raise(lf_error_t error, const char *format, ...) {
	lf_error_t _error = error;
	/* Record the observed error. */
	flipper.error_code = error;
#ifdef __enable_error_side_effects__
	if(flipper.errors_cause_side_effects && error)
	{
		/* Construct a va_list to access variadic arguments. */
		va_list argv;
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
#endif
	return;
}

lf_error_t error_get(void) {
	return flipper.error_code;
}

void error_clear(void) {
	flipper.error_code = E_OK;
}

void error_resume(void) {
	flipper.errors_cause_side_effects = 1;
}

void error_pause(void) {
	flipper.errors_cause_side_effects = 0;
}
