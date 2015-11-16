#ifndef __fs_h__

#define __fs_h__

#include <flipper/types.h>

#define _FREE_LIST 32

extern fsp _free_list;

#define _BREAK_VALUE 64

extern fsp _break_value;

#define _ROOT_LEAF 128

extern fsp _root_leaf;

/* A local copy of the root pointer. */

extern fsp root;

extern const struct _fs {
	
	void (* configure)(void);
	
	void (* format)(void);
	
} fs;

#ifdef __private_include__

enum { _fs_configure, _fs_format };

extern void fs_configure(void);

extern void fs_format(void);

extern void fs_print(fsp branch);

void fs_transfer_file(char *path, char *name);

void fs_download_file(char *name, char *path);

#endif

#endif