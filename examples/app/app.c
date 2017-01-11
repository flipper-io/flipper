/* Include the application header file. */
#include "app.h"

void test() {

}

extern const struct _app {
    void (* test)(void);
} app;

const struct _app app __attribute__((section (".module"))) = {
    test
};

/* Initialize the data section. */
int counter = 50;

/* Defining the 'main' symbol is what separates modules from applications. */
void main(void) {
    PIOA -> PIO_PER = PIO_PA16;
    PIOA -> PIO_OER = PIO_PA16;
    PIOA -> PIO_OWER = PIO_PA16;
    while (1) {
        PIOA -> PIO_ODSR ^= PIO_PA16;
        for (int i = 0; i < 1000000; i ++) __ASM volatile ("nop");
    }
}
