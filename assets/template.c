#include <flipper.h>

struct _PACKAGE {
STRUCTDEF
};

enum { TAGS };

FUNCTIONPROTOS

VARIABLES

#ifdef __ATSAM4S__

const char _fmr_app_name[] __attribute__((section (".name"))) = "PACKAGE";

#define JT_SECTION __attribute__((section (".module")))

#else

extern uint8_t package_bin[];
extern size_t package_bin_len;

LF_MODULE(_module, "PACKAGE", "DESCRIPTION", &package_bin, &package_bin_len);

FUNCTIONS

#endif

const struct _PACKAGE _jumptable JT_SECTION= {
STRUCTBODY
};
