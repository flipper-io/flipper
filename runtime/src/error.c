#include <flipper.h>

#include <flipper/error.h>

/* Expose the error message strings. */
char *lf_error_messages[] = { LF_ERROR_MESSAGE_STRINGS };
char last_error[256];
lf_error_t error_code = E_OK;
#ifdef __no_err_str__
uint8_t errors_cause_side_effects = 0;
#else
uint8_t errors_cause_side_effects = 1;
#endif

int lf_error_configure(void) {
	return lf_success;
}

void lf_error_raise(lf_error_t error, const char *format, ...) {
	/* Record the observed error. */
	error_code = error;
#ifndef __no_err_str__
    if (lf_debug_level > LF_DEBUG_LEVEL_OFF) {
		lf_error_t _error = error;
		if (error && errors_cause_side_effects) {
			/* Construct a va_list to acmmcess variadic arguments. */
			va_list argv;
			/* Initialize the va_list that we created above. */
			va_start(argv, format);
			if (_error > (sizeof(lf_error_messages) / sizeof(char *))) {
				_error = E_STRING;
			}
			/* Get the variadic argument string. */
			vsprintf(last_error, format, argv);
			/* Print the exception if a message is provided. */
			if (format) {
				fprintf(stderr, KYEL "\nThe Flipper runtime encountered the following error:\n  " KNRM "↳ " KRED);
				if (_error == E_STRING) {
					fprintf(stderr, "An invalid error code (%i) was provided.\n", error);
				} else {
					fprintf(stderr, "%s\n", last_error);
				}
			}
			/* Release the va_list. */
			va_end(argv);
			if (errors_cause_side_effects) {
				if (_error >= E_MAX) {
					_error = E_UNIMPLEMENTED;
				}
				/* Print the error code. */
				fprintf(stderr, KNRM "Error code (%i): '" KBLU "%s" KNRM "'\n\n", _error, lf_error_messages[_error]);
			}
		}
	}
#endif
	return;
}

char *lf_error_string(void) {
	return last_error;
}

lf_error_t lf_error_get(void) {
	return error_code;
}

void lf_error_clear(void) {
	error_code = E_OK;
}

void lf_error_resume(void) {
	errors_cause_side_effects = 1;
}

void lf_error_pause(void) {
	errors_cause_side_effects = 0;
}
