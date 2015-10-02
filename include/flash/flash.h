#ifndef __flash_h__

#define __flash_h__

#include <flipper/types.h>

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

/* ~ Helper functions. ~ */

extern void flash_transfer_page_to_buffer_with_erase(uint16_t page, uint8_t buffer);

extern void flash_transfer_buffer_to_page_with_erase(uint8_t buffer, uint16_t page, bool erase);

extern void flash_begin_writing_to_buffer_with_offset(uint8_t buffer, uint16_t offset);

extern void flash_begin_continuous_read(uint16_t page, uint16_t offset);

#endif

#endif