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
//<<<<<<< HEAD
//=======
//
//extern int _write(int file, char *ptr, int len) {
//	for (int i = 0; i < len; i ++, ptr ++) {
//		uart0_put(*ptr);
//	}
//	return 0;
//}

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
