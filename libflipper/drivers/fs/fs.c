#define __private_include__

#include <fs/fs.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

#include <flash/flash.h>

void fs_format(void) {
	
	device.invoke(_fs, _fs_format, NO_ARGS);
	
	_free_list = 0;

	_break_value = 528;

	flash.pull(&_root_leaf, sizeof(fsp), _ROOT_LEAF);
		
}