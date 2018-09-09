#include "libflipper.h"
#include "atmegau2.h"
#include "megausb.h"

int megausb_interrupt_receive(void *dst, uint32_t length) {
    return lf_error;
}

int megausb_interrupt_transmit(void *src, uint32_t length) {
    return lf_error;
}
