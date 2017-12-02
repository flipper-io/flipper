#include <PACKAGE.h>

LF_MODULE(_PACKAGE, "PACKAGE", "DESCRIPTION");

VARIABLES

const struct _PACKAGE PACKAGE = {
STRUCT
};

int PACKAGE_configure(void) {
	return lf_bind(&_app);
}

FUNCTIONS
