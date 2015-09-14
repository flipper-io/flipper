#ifndef __flash_h__

#define __flash_h__

#include <types.h>

extern const struct _flash {
	
	void (* configure)(void);
	
	void (* enable)(void);
	
	void (* disable)(void);
	
	void (* reset)(void);
	
	fsp (* alloc)(uint32_t length);
	
	void (* free)(fsp pointer);
	
	void (* format)(void);
	
	void (* push)(void *source, uint32_t length, fsp destination);
	
	void (* pull)(void *destination, uint32_t length, fsp source);
	
	void *(* dereference)(fsp source, uint32_t length);
	
} flash;

#ifdef __private_include__

enum { _flash_configure, _flash_enable, _flash_disable, _flash_reset, _flash_alloc, _flash_free, _flash_format, _flash_push, _flash_pull, _flash_dereference };

extern void flash_configure(void);

extern void flash_enable(void);

extern void flash_disable(void);

extern void flash_reset(void);

extern fsp flash_alloc(uint32_t length);

extern void flash_free(fsp pointer);

extern void flash_format(void);

extern void flash_push(void *source, uint32_t length, fsp destination);

extern void flash_pull(void *destination, uint32_t length, fsp source);

extern void *flash_dereference(fsp source, uint32_t length);

#endif

#endif