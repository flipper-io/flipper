#include "libflipper.h"

enum { _temp_configure };

int temp_configure(void);

void *temp_interface[] = { &temp_configure };

LF_MODULE(temp);

LF_WEAK int temp_configure(void) {
    lf_return_t retval;
    lf_invoke(lf_get_selected(), "temp", _temp_configure, lf_int_t, &retval, NULL);
    return (int)retval;
}
