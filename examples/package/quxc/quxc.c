#define __private_include__
#include <qux/qux.h>

LF_MODULE(_qux, "qux", "A simple test module.", -1);

int qux_configure(void) {
    /* Bind this module to its counterpart on the device. */
    lf_bind(&_qux);
    /* Invoke the function `qux_configure` on the device. */
    return lf_invoke(&_qux, _qux_configure, NULL);
}

void qux_test(void) {
    /* Invoke the function `qux_test` on the device. */
    lf_invoke(&_qux, _qux_test, NULL);
}
