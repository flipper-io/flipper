#ifndef __fs_h__
#define __fs_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Include header dependancies for the filesystem. ~ */
#include <flipper/fs/crc.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _fs {

	void (* configure)(void);
	void (* format)(void);
//	void (* create)(char *name, void *data, uint32_t length);
//	void (* delete)(char *name);
//  void (* rename)(char *name, char *new);
//  void (* append)(char *name, void *data, uint32_t length);
//  void (* put)(void);
//  void (* get)(void);
	fsp (* data)(char *name);

#ifndef __osmium__
	fsp (* upload)(char *path, char *name);
	void (* download)(char *name, char *path);
#endif

} fs;

#ifdef __private_include__

/* ~ Declare all private macros. ~ */
#define _FREE_LIST	 32
#define _BREAK_VALUE 36
#define _ROOT_LEAF	 40

#define DEFAULT_BREAK_VALUE 0x1234

/* ~ Declare all global variables. ~ */
extern fsp _free_list;
extern fsp _break_value;
extern fsp _root_leaf;

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _fs_configure, _fs_format };

/* ~ Declare all function prototypes for this driver. ~ */
void fs_configure(void);
void fs_format(void);

/* ~ Returns a filesystem pointer to the data of the specified file. ~ */
fsp fs_data(char *name);

fsp fs_upload(char *path, char *name);
void fs_download(char *name, char *path);

#endif
#endif
