/* Include the application header file. */
#include "app.h"

/* Initialize the data section. */
int counter = 100;

/* Defining the 'main' symbol is what separates modules from applications. */
void main(void) {
    PIOA -> PIO_PER = PIO_PA0;
    PIOA -> PIO_OER = PIO_PA0;
    PIOA -> PIO_OWER = PIO_PA0;
    while (counter --) {
        PIOA -> PIO_ODSR ^= PIO_PA0;
        for (int i = 0; i < 500000; i ++) __ASM volatile ("nop");
    }
}
