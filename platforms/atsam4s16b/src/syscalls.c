#include <sys/stat.h>
#include <sys/types.h>

extern int errno;
extern int _end;

extern caddr_t _sbrk (int incr) {
	return 0;
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

extern int _write( int file, char *ptr, int len ) {
	return 0;
}

extern void _exit(int status) {
	while (1);
}

extern void _kill(int pid, int sig) {
	return;
}

extern int _getpid (void) {
	return -1;
}

extern void end(void) {
	return;
}
