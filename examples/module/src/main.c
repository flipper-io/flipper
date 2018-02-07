#include "main.h"

#define PIN IO_1

LF_FUNC int my_func(uint8_t a) {
    PIOA->PIO_PER = PIN;
	PIOA->PIO_OER = PIN;
	PIOA->PIO_OWER = PIN;
	PIOA->PIO_ODSR ^= PIN;
    return 0;
}
