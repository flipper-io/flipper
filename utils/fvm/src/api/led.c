#include <flipper/flipper.h>

int led_configure(void) {
    printf("Configured the led.\n");
    return lf_success;
}

void led_rgb(uint8_t r, uint8_t g, uint8_t b) {
    printf("Setting the led color to R: %i, G: %i, B: %i.\n", r, g, b);
}
