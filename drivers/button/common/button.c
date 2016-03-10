#define __private_include__
#include <button/button.h>

/* ~ Define the virtual driver object. ~ */
const struct _button button = {

	button_configure,
	button_read

};