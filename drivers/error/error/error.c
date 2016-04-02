#define __private_include__
#include <flipper/error.h>
#include <flipper/fmr.h>

char *error_messages[] = { ERROR_STRING_ARRAY };

uint8_t error_disclosed = 0;
uinterror_t error_code = E_OK;

void error_configure(void) {
	return;
}

void error_withold(void) {
	error_disclosed = 0;
	return;
}

void error_disclose(void) {
	error_disclosed = 1;
	return;
}

void error_raise(uinterror_t code, char *format, ...) {

	/* ~ Construct a va_list to access variadic arguments. ~ */
	va_list argv;

	/* ~ Initialize the va_list that we created above. ~ */
	va_start(argv, format);

	if(error_disclosed) {

		/* ~ Save the error code into the global error state. ~ */
		error_code = code;

	}

	else {

		/* ~ Print the error message to stderror. ~ */
		vfprintf(stderr, format, argv);

		if(code < E_GREATEST) {
			fprintf(stderr, "\n\nError code (%i): %s\n\n", code, error_messages[code]);
		}

		else {
			fprintf(stderr, "\n\nUser defined error occured.\n\n");
		}

		/* ~ Exit the instance of libflipper. ~ */
		exit(EXIT_FAILURE);

	}

	/* ~ Release the variadic argument list. ~ */
	va_end(argv);

}

uinterror_t error_get(void) {

	return error_code;

}

void error_clear(void) {
	error_code = E_OK;
	return;
}
