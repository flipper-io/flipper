#include "main.h"

LF_FUNC int my_func(uint8_t a) {
    PIOA->PIO_PER = PIO_PA8;
	PIOA->PIO_OER = PIO_PA8;
	PIOA->PIO_OWER = PIO_PA8;
	PIOA->PIO_ODSR ^= PIO_PA8;
    return 0;
}
