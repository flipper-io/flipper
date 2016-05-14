#define __private_include__
#include <flipper/fs.h>
#include <flipper/nvm.h>
#include <flipper/platform/platform.h>

void fs_configure(void) {
	
	/* We have to load the freelist and the _break_value in from memory! */
	nvm_pull(&_free_list, sizeof(fsp), _FREE_LIST);
	nvm_pull(&_break_value, sizeof(fsp), _BREAK_VALUE);
	nvm_pull(&_root_leaf, sizeof(fsp), _ROOT_LEAF);
	
}

void fs_format(void) {

}

fsp fs_data(char *name) {

	return 0;

}