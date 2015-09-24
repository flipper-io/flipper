#ifndef __fs_h__

#define __fs_h__

#include <types.h>

/* A simple data structure used to represent files. */

typedef struct _file {
	
	/* The amount of data held within the file. */
	
	uint32_t size;
	
	/* A pointer to the data held within the file. */
	
	fsp data;
	
	/* The length of the name of the file. */
	
	uint8_t namelen;
	
	/* The name of the file. */
	
	char name[];
	
} file;

#define _ROOT 0

/* A local copy of the root pointer. */

extern fsp root;

extern const struct _fs {
	
	void (* configure)(void);
	
} fs;

#ifdef __private_include__

enum { _fs_configure };

void fs_configure(void);

#endif

#endif