#define __private_include__

#include <fs/fs.h>

#include <flash/flash.h>

#include <fs/tree.h>

#import <fmr/fmr.h>

#import <platform/fmr.h>

void fs_configure(void) {
	
	/* ~ We have to load the freelist and the _break_value in from memory! ~ */
	
	flash.pull(&_free_list, sizeof(fsp), _FREE_LIST);
	
	flash.pull(&_break_value, sizeof(fsp), _BREAK_VALUE);
	
	flash.pull(&_root_leaf, sizeof(fsp), _ROOT_LEAF);
	
}

void fs_format(void) {
	
	device.invoke(_fs, _fs_format, NO_ARGS);
	
	_free_list = 0;
	
	_break_value = 528;
	
	flash.pull(&_root_leaf, sizeof(fsp), _ROOT_LEAF);
	
}

