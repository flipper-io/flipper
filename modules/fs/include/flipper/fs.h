/* fs.h - Provide a standard for interacting with files on an lf_device. */

#ifndef __lf_fs_h__
#define __lf_fs_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* Computes the offset of a member of a data structure located at a given nvm_p. */
#define fs_access(address, structure, member) (nvm_p)(address + offsetof(structure, member))

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
	/* Creates a file with the given name and file size. */
	int (* create)(char *name, lf_size_t size);
	/* Removes a file with the given name. */
	int (* delete)(char *name);
	/* Opens a r/w session starting an a specific offset from a file with the given name. */
	int (* open)(char *name, lf_size_t offset);
	/* Returns the size of the open file. */
	lf_size_t (* size)(void);
	/* Seeks to a given offset in the open file. */
	void (* seek)(lf_size_t offset);
	/* Reads a single byte from the file if a read session is active. */
	uint8_t (* get)(void);
	/* Pushes data into the open file. */
	void (* push)(void *source, lf_size_t length);
	/* Pulls from the open file. */
	void (* pull)(void *destination, lf_size_t length);
	/* Closes the open file. */
	void (* close)(void);
	/* Permanently destroys all records catalogued by the filesystem. */
	void (* format)(void);
/* If this flag is turned on, symbols to move data from the host's file system to the device's will be enabled. */
#ifdef __fs_transfer_symbols__
	/* Copies a file from the host's filesystem to the device's filesystem. */
	int (* transfer)(char *path, char *name);
	/* Copies a file from the device's filesystem to the host's filesystem. */
	int (* receive)(char *name, char *path);
#endif
} fs;

#ifdef __private_include__

/* Declare the message runtime overlay for this driver. */
enum { _fs_configure, _fs_create, _fs_delete, _fs_size, _fs_write, _fs_put, _fs_read, _fs_get, _fs_push, _fs_pull, _fs_close, _fs_format };

/* ~ Define types and macros internal to this driver. ~ */
#define _FREE_LIST	 32
#define _BREAK_VALUE 36
#define _ROOT_LEAF	 40

/* ~ Declare internal global state. ~ */
extern nvm_p _free_list;
extern nvm_p _break_value;
extern nvm_p _root_leaf;
extern nvm_p _rw_head;

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
extern int fs_configure(void);
extern int fs_create(char *name, lf_size_t size);
extern int fs_delete(char *name);
extern int fs_open(char *name, lf_size_t offset);
extern lf_size_t fs_size(char *name);
extern void fs_seek(lf_size_t offset);
extern uint8_t fs_get(void);
extern void fs_push(void *source, lf_size_t length);
extern void fs_pull(void *destination, lf_size_t length);
extern void fs_close(void);
extern void fs_format(void);

#ifdef __fs_transfer_symbols__
extern int fs_transfer(char *path, char *name);
extern int fs_receive(char *name, char *path);
#endif

/* ~ Declare the prototypes for the supporting functions belonging to this driver. ~ */
nvm_p fs_add_leaf_with_key(nvm_p current, uint16_t key);
nvm_p fs_leaf_for_key(nvm_p current, uint16_t key);
int fs_remove_leaf_with_key(nvm_p parent, uint16_t key);

#endif
#endif
