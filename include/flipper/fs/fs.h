#ifndef __fs_h__
#define __fs_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/flipper/core.h>

/* ~ Include header dependancies for the filesystem. ~ */
#include "crc.h"

/* ~ Declare the virtual driver object. ~ */
extern const struct _fs {

	void (* configure)(void);
	void (* format)(void);

} fs;

#ifdef __private_include__

/* ~ Declare all private macros. ~ */
#define _FREE_LIST	 32
#define _BREAK_VALUE 36
#define _ROOT_LEAF	 40

/* ~ Declare all global variables. ~ */
extern fsp _free_list;
extern fsp _break_value;
extern fsp _root_leaf;

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _fs_configure, _fs_format };

/* ~ Declare all function prototypes for this driver. ~ */
extern void fs_configure(void);
void fs_transfer_file(char *path, char *name);
void fs_download_file(char *name, char *path);
extern void fs_format(void);

#endif
#endif
