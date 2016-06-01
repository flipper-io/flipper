#ifndef __nvm_h__
#define __nvm_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

#define NVM_PAGE_SIZE 528

/* Declare the virtual interface for this module. */
extern const struct _nvm {
	void (* configure)(void);
	void (* enable)(void);
	void (* disable)(void);
	void (* reset)(void);
	void (* read)(fsp address);
	uint8_t (* get)(void);
	fsp (* alloc)(fmr_size_t length);
	void (* free)(fsp pointer);
	void (* format)(void);
	void (* push)(void *source, fmr_size_t length, fsp destination);
	void (* pull)(void *destination, fmr_size_t length, fsp source);
	void *(* dereference)(fsp source, fmr_size_t length);
} nvm;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _nvm_configure, _nvm_enable, _nvm_disable, _nvm_reset, _nvm_read, _nvm_get, _nvm_alloc, _nvm_free, _nvm_format, _nvm_push, _nvm_pull, _nvm_dereference };

/* Declare each prototype for all functions within this driver. */
void nvm_configure(void);
void nvm_enable(void);
void nvm_disable(void);
void nvm_reset(void);
void nvm_read(fsp address);
uint8_t nvm_get(void);
fsp nvm_alloc(fmr_size_t length);
void nvm_free(fsp pointer);
void nvm_format(void);
void nvm_push(void *source, fmr_size_t length, fsp destination);
void nvm_pull(void *destination, fmr_size_t length, fsp source);
void *nvm_dereference(fsp source, fmr_size_t length);

/* Declare all necessary private driver functions. */
void nvm_transfer_page_to_buffer_with_erase(uint16_t page, uint8_t buffer);
void nvm_transfer_buffer_to_page_with_erase(uint8_t buffer, uint16_t page, uint8_t erase);
void nvm_begin_writing_to_buffer_with_offset(uint8_t buffer, uint16_t offset);
void nvm_begin_continuous_read(uint16_t page, uint16_t offset);

#endif
#endif
