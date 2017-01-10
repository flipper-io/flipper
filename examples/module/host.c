#define __private_include__
#include <module/module.h>

LF_MODULE(_module, "module", "A simple test module.", -1);

int module_configure(void) {
    printf("Module configure.\n");
    /* Bind the module to libflipper. */
    _module.index = 0;
    return lf_invoke(&_module, _module_configure, NULL);
}

void module_test(void) {
    printf("Module test.\n");
    lf_invoke(&_module, _module_test, NULL);
}
