#include <flipper.h>
#include <sys/stat.h>
#include <sys/types.h>

extern int errno;
extern int _end;

extern caddr_t _sbrk(int increment) {
	static unsigned char *heap = NULL ;
	unsigned char *previous;

	if (heap == NULL) {
		heap = (unsigned char *)&_end;
	}

	previous = heap;

	heap += increment;

	return (caddr_t)(previous);
}

extern void uart0_put(uint8_t c);

extern int _close(int file) {
	return -1 ;
}

extern int _fstat(int file, struct stat *st) {
	return 0 ;
}

extern int _isatty(int file) {
	return 1 ;
}

extern int _lseek(int file, int ptr, int dir) {
	return 0 ;
}

extern int _read(int file, char *ptr, int len) {
	return 0 ;
}

extern int _write(int file, char *ptr, int len) {
	uint32_t state = PIOA->PIO_ODSR & FMR_PIN;
	PIOA->PIO_SODR = FMR_PIN;
	while (len --) uart0_put(*ptr++);
	/* Wait a bit before raising the FMR pin. */
	for (size_t i = 0; i < 0x3FF; i ++) __asm__ __volatile__("nop");
	if (state) {
		PIOA->PIO_SODR = FMR_PIN;
	} else {
		PIOA->PIO_CODR = FMR_PIN;
	}
	return 0;
}

extern void _exit(int status) {
	while (1);
}

extern void _kill(int pid, int sig) {
	return;
}

extern int _getpid(void) {
	return -1;
}

extern void end(void) {
	return;
}
