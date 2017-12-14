#include <flipper.h>

struct _PACKAGE {
STRUCTDEF
};

enum { TAGS };

FUNCTIONPROTOS

#ifdef __DEVICE__

const char _fmr_app_name[] __attribute__((section (".name"))) = "PACKAGE";

VARIABLES

const struct _PACKAGE PACKAGE __attribute__((section (".module"))) = {
STRUCTBODY
};

#else

extern uint8_t package_bin[];
extern size_t package_bin_len;

LF_MODULE(_PACKAGE, "PACKAGE", "DESCRIPTION", &package_bin, &package_bin_len);

FUNCTIONS

#endif
