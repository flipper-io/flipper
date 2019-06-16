#include "libflipper.h"
#include "button.h"
#include "atmegau2.h"

LF_FUNC int button_configure(void) {
    BUTTON_DDR &= ~(1 << BUTTON_PIN);
    return lf_success;
}

LF_FUNC uint8_t button_read(void) {
    return ((BUTTON_IN & (1 << BUTTON_PIN)) >> BUTTON_PIN);
}
