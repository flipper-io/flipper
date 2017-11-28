#define __private_include__
#include <flipper.h>

int main(void) {

	/* Loop here if the kernel were ever to reach an unknown execution state. */
	while (1) __asm__ __volatile__("nop");
}
