#define __private_include__
#include <module/module.h>

LF_MODULE(_module, "module", "A simple test module.", -1);

int module_configure(void) {
    printf("Module configure.\n");
    /* Hard code the module as a user module with index 0. */
    _module.index = 0x00 | FMR_USER_INVOCATION_BIT;
    return lf_invoke(&_module, _module_configure, NULL);
}

void module_test(void) {
    printf("Module test.\n");
    lf_invoke(&_module, _module_test, NULL);
}
