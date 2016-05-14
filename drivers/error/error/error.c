#define __private_include__
#include <flipper/error.h>

char *error_messages[] = { ERROR_STRING_ARRAY };

void error_configure(void) {

}

void error_withold(void) {

	error_disclosed = 0;

}

void error_disclose(void) {

	error_disclosed = 1;

}

void error_raise(lf_error_t code, char *format, ...) {

	/* Construct a va_list to access variadic arguments. */
	va_list argv;

	/* Initialize the va_list that we created above. */
	va_start(argv, format);

	if (error_disclosed) {

		/* Save the error code into the global error state. */
		error_code = code;

	}

	else {

		/* Print the error message to stderror. */
		vfprintf(stderr, format, argv);

		if (code < E_GREATEST) {
			fprintf(stderr, "\n\nError code (%i): %s\n\n", code, error_messages[code]);
		}

		else {
			fprintf(stderr, "\n\nUser defined error occured.\n\n");
		}

		/* Exit the instance of libflipper. */
		exit(EXIT_FAILURE);

	}

	/* Release the variadic argument list. */
	va_end(argv);

}

lf_error_t error_get(void) {

	return error_code;

}

void error_clear(void) {

	error_code = E_OK;

}
