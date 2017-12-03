#define __private_include__
#include <qux/qux.h>

LF_MODULE(_qux, "qux", "A simple test module.");

int qux_configure(void) {
    /* Bind this module to its counterpart on the device. */
    lf_bind(&_qux);
    /* Invoke the function `qux_configure` on the device. */
    return lf_invoke(&_qux, _qux_configure, fmr_int_t, NULL);
}

void qux_test(void) {
    /* Invoke the function `qux_test` on the device. */
    lf_invoke(&_qux, _qux_test, fmr_int_t, NULL);
}
