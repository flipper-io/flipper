#include "libflipper.h"

enum { _swd_configure };

int swd_configure(void);

void *swd_interface[] = { &swd_configure };

LF_MODULE(swd);

LF_WEAK int swd_configure(void) {
    lf_return_t retval;
    lf_invoke(lf_get_selected(), "swd", _swd_configure, lf_int_t, &retval, NULL);
    return (int)retval;
}
