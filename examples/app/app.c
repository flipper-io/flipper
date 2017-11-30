/* Include the application header file. */
#include "app.h"

const char _fmr_app_name[] __attribute__((section (".name"))) = "app"; 

/* Initialize the data section. */
int counter = 50;

/* Defining the 'main' symbol is what separates modules from applications. */
void main(void) {
    PIOA -> PIO_PER = PIO_PA8;
    PIOA -> PIO_OER = PIO_PA8;
    PIOA -> PIO_OWER = PIO_PA8;
    while (1) {
        PIOA -> PIO_ODSR ^= PIO_PA8;
        for (int i = 0; i < 1000000; i ++) __asm__ __volatile__ ("nop");
    }
}
