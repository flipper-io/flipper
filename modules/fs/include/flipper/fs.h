/* fs.h - Provide a standard for interacting with files on an lf_device. */

#ifndef __lf_fs_h__
#define __lf_fs_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* Computes the offset of a member of a data structure located at a given nvm_p. */
#define lf_forward(address, structure, member) (nvm_p)(address + offsetof(structure, member))

/* An abstract data structure used to represent members of the filesystem tree. */
typedef struct __attribute__((__packed__)) _leaf {
	/* A key used to identify the leaf. */
	lf_id_t key;
	/* The size of the data pointed to by the leaf. */
	uint32_t size;
	/* The data pointed to by the leaf. */
	nvm_p data;
	/* A pointer that points to the child pointer of the parent that points to this leaf. */
	nvm_p _branch;
	/* A pointer to the leftmost child of the leaf. */
	nvm_p left;
	/* A pointer to the rightmost child of the leaf. */
	nvm_p right;
} leaf;

/* ~ Declare the virtual interface for this driver. ~ */
extern const struct _fs {
	/* Configures the filesystem. */
	int (* configure)(void);
	/* Permanently destroys all records catalogued by the filesystem. */
	void (* format)(void);
	/* Creates a file with the name and data given. */
	int (* create)(char *name, void *data, size_t length);
	/* Removes a file with the given name. */
	int (* remove)(char *name);
	/* Renames a file. */
	int (* rename)(char *from, char *to);
	/* Opens a write session to the beginning of the file with the name given. */
	void (* write)(char *name);
	/* Wraps the NVM function used to write a byte to the filesystem. */
	void (* put)(uint8_t byte);
	/* Opens a read session from the beginning of the file with the name given. */
	void (* read)(char *name);
	/* Wraps the NVM function used to read a byte from the filesystem. */
	uint8_t (* get)(void);
	/* Returns a pointer to a file's data within NVM. */
	nvm_p (* data)(char *name);
/* If this flag is turned on, symbols to move data from the host's file system to the device's will be enabled. */
#ifdef __fs_transfer_symbols__
	/* Copies a file from the host's filesystem to the device's filesystem. */
	nvm_p (* transfer)(char *path, char *name);
	/* Copies a file from the device's filesystem to the host's filesystem. */
	void (* receive)(char *name, char *path);
#endif
} fs;

#ifdef __private_include__

/* Declare the message runtime overlay for this driver. */
enum { _fs_configure, _fs_format, _fs_create, _fs_remove, _fs_rename, _fs_write, _fs_put, _fs_read, _fs_get, _fs_data };

/* ~ Define types and macros internal to this driver. ~ */
#define _FREE_LIST	 32
#define _BREAK_VALUE 36
#define _ROOT_LEAF	 40

/* ~ Declare internal global state. ~ */
extern nvm_p _free_list;
extern nvm_p _break_value;
extern nvm_p _root_leaf;

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
extern int fs_configure(void);
extern void fs_format(void);
extern int fs_create(char *name, void *data, size_t length);
extern int fs_remove(char *name);
extern int fs_rename(char *from, char *to);
extern void fs_write(char *name);
/* fs.put maps to nvm_put */
extern void fs_read(char *name);
/* fs.get maps to nvm_get */
extern nvm_p fs_data(char *name);
extern nvm_p fs_transfer(char *path, char *name);
extern void fs_receive(char *name, char *path);

/* ~ Declare the prototypes for the supporting functions belonging to this driver. ~ */
nvm_p fs_add_leaf_with_key(nvm_p current, uint16_t key);
nvm_p fs_leaf_for_key(nvm_p current, uint16_t key);
int fs_remove_leaf_with_key(nvm_p parent, uint16_t key);

#endif
#endif
