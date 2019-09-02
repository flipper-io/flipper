#include "libflipper.h"

LF_FUNC int button_configure(void) {
    return lf_success;
}

LF_FUNC uint8_t button_read(void) {
    printf("Reading button value.\n");
    return false;
}
