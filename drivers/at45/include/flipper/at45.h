#ifndef __at45_h__
#define __at45_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

#define AT45_PAGE_SIZE 528

/* ~ Declare the virtual driver object. ~ */
extern const struct _at45 {

	void (* configure)(void);
	void (* enable)(void);
	void (* disable)(void);
	void (* reset)(void);
	void (* read)(fsp address);
	uint8_t (* get)(void);
	fsp (* alloc)(uint32_t length);
	void (* free)(fsp pointer);
	void (* format)(void);
	void (* push)(void *source, size_t length, fsp destination);
	void (* pull)(void *destination, size_t length, fsp source);
	void *(* dereference)(fsp source, size_t length);

} at45;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _at45_configure, _at45_enable, _at45_disable, _at45_reset, _at45_read, _at45_get, _at45_alloc, _at45_free, _at45_format, _at45_push, _at45_pull, _at45_dereference };

/* ~ Declare all function prototypes for this driver. ~ */
void at45_configure(void);
void at45_enable(void);
void at45_disable(void);
void at45_reset(void);
void at45_read(fsp address);
uint8_t at45_get(void);
fsp at45_alloc(uint32_t length);
void at45_free(fsp pointer);
void at45_format(void);
void at45_push(void *source, size_t length, fsp destination);
void at45_pull(void *destination, size_t length, fsp source);
void *at45_dereference(fsp source, size_t length);

/* ~ Declare all necessary private driver functions. ~ */
void at45_transfer_page_to_buffer_with_erase(uint16_t page, uint8_t buffer);
void at45_transfer_buffer_to_page_with_erase(uint8_t buffer, uint16_t page, uint8_t erase);
void at45_begin_writing_to_buffer_with_offset(uint8_t buffer, uint16_t offset);
void at45_begin_continuous_read(uint16_t page, uint16_t offset);

#endif
#endif
