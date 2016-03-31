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
	fsp (* alloc)(uint32_t length);
	void (* free)(fsp pointer);
	void (* format)(void);
	void (* push)(void *source, uint32_t length, fsp destination);
	void (* pull)(void *destination, uint32_t length, fsp source);
	void *(* dereference)(fsp source, uint32_t length);

} at45;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _at45_configure, _at45_enable, _at45_disable, _at45_reset, _at45_alloc, _at45_free, _at45_format, _at45_push, _at45_pull, _at45_dereference };

/* ~ Declare all function prototypes for this driver. ~ */
extern void at45_configure(void);
extern void at45_enable(void);
extern void at45_disable(void);
extern void at45_reset(void);
extern void read(fsp address);
extern fsp at45_alloc(uint32_t length);
extern void at45_free(fsp pointer);
extern void at45_format(void);
extern void at45_push(void *source, uint32_t length, fsp destination);
extern void at45_pull(void *destination, uint32_t length, fsp source);
extern void *at45_dereference(fsp source, uint32_t length);

/* ~ Declare all necessary private driver functions. ~ */
extern void at45_transfer_page_to_buffer_with_erase(uint16_t page, uint8_t buffer);
extern void at45_transfer_buffer_to_page_with_erase(uint8_t buffer, uint16_t page, bool erase);
extern void at45_begin_writing_to_buffer_with_offset(uint8_t buffer, uint16_t offset);
extern void at45_begin_continuous_read(uint16_t page, uint16_t offset);

#endif
#endif
