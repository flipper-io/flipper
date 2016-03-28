#define __private_include__
#include <flipper/error/error.h>

/* ~ Define the virtual driver object. ~ */
struct _error error = {

	(const void (*)(void))error_configure,
	(const void (*)(void))error_withold,
	(const void (*)(void))error_disclose,
	(const void (*)(uinterror_t code, char *string))error_raise,
	(const void (*)(void))error_clear,
	0,
	E_OK

};
