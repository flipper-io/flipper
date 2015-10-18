#define __private_include__

#include <fs/fs.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

void fs_format(void) {
	
	device.invoke(_fs, _fs_format, NO_ARGS);
	
	fs_configure();
	
}