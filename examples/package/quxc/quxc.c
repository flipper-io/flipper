#define __private_include__
#include <qux/qux.h>

LF_MODULE(_qux, "qux", "A simple test module.", -1);

int qux_configure(void) {
    printf("qux configure.\n");
    /* Hard code the module as a user module with index 0 for now. */
    _qux.index = 0x00 | FMR_USER_INVOCATION_BIT;
    /* Invoke the function `qux_configure` on the device. */
    return lf_invoke(&_qux, _qux_configure, NULL);
}

void qux_test(void) {
    printf("qux test.\n");
    /* Invoke the function `qux_test` on the device. */
    lf_invoke(&_qux, _qux_test, NULL);
}
