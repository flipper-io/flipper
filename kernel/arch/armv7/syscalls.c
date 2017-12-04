#include <sys/stat.h>
#include <sys/types.h>
#define __private_include__
#include <flipper/usart.h>
#include <flipper/gpio.h>
#include <flipper/atsam4s/atsam4s.h>

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

extern int link(char *old, char *new) {
	return -1 ;
}

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

extern void uart0_put(char c);

extern int _write(int file, char *ptr, int len) {
	int pin = 0;
	if (gpio_read(FMR_PIN)) {
		pin = 1;
		gpio_write(0, FMR_PIN);
	}
	for (int i = 0; i < len; i ++, ptr ++) {
		uart0_put(*ptr);
	}
	if (pin) {
		gpio_write(FMR_PIN, 0);
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
