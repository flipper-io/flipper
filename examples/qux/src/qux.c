#define __private_include__
#include <qux/qux.h>=

/* -- BEGIN GENERATED -- */

struct _qux qux = {
    qux_configure,
    qux_a
};

struct _lf_module *__fld_module_address = &_qux;

/* -- END GENERATED -- */

LF_MODULE(_qux, "qux", "Does qux stuff.", 0);

int qux_configure(void) {
    /* Bind this module to its counterpart on the device. */
    lf_bind(&_qux);
    return lf_success;
}

void qux_a(void) {
	printf("Hello from qux_a.\n");
	printf("My identifier is 0x%04x\n", _qux.identifier);
	printf("My slot is 0x%02x\n", _qux.slot);
}
