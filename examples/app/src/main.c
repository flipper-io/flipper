#include "main.h"

const char _name[] __attribute__((section(".name"))) = "example";

#define PIN IO_1

void delay(int time) {
    for (volatile uint64_t i = 0; i < time * 10000; i ++);
}

int main(int argc, char *argv[]) {
    PIOA->PIO_PER = PIN;
	PIOA->PIO_OER = PIN;
	PIOA->PIO_OWER = PIN;
	while (1) {
        PIOA->PIO_ODSR ^= PIN;
        delay(100);
    }
    return 0;
}
