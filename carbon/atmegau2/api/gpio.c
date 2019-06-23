#include "libflipper.h"
#include "atmegau2.h"

LF_FUNC int gpio_configure(void) {
    return lf_success;
}

LF_FUNC void gpio_enable(uint32_t enable, uint32_t disable) {
    DDRD |= enable & 0xff;
    DDRD &= ~(disable & 0xff);
}

LF_FUNC void gpio_write(uint32_t set, uint32_t clear) {
    PORTD |= set & 0xff;
    PORTD &= ~(clear & 0xff);
}

LF_FUNC uint32_t gpio_read(uint32_t mask) {
    return (uint32_t)PIND;
}
