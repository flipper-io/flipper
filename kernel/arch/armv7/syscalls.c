#include <sys/stat.h>
#include <sys/types.h>
#include <flipper/usart.h>
#include <flipper/gpio.h>

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

extern void uart0_put(char c);

extern int _write(int file, char *ptr, int len) {
	for (int i = 0; i < len; i ++, ptr ++) {
		uart0_put(*ptr);
	}
	return 0;
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
