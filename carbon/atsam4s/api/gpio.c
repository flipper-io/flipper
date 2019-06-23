#include "libflipper.h"

LF_FUNC int gpio_configure(void) {
    return lf_success;
}

LF_FUNC void gpio_enable(uint32_t enable, uint32_t disable) {
}

LF_FUNC void gpio_write(uint32_t set, uint32_t clear) {
}

LF_FUNC uint32_t gpio_read(uint32_t mask) {
    return 0;
}
