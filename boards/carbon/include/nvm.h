#ifndef __nvm_h__
#define __nvm_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/libflipper.h>

/* ~ Define types and macros exposed by this driver. ~ */

/* Defines the size of a page within non-volatile memory. */
#define NVM_PAGE_SIZE 528
/* Converts a page and offset within NVM to an address within NVM. */
#define nvm_address_from_page_and_offset(page, offset) (page * NVM_PAGE_SIZE) + offset

#ifdef __private_include__

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
int nvm_configure(void);
/* Enables interaction with external memory. */
void nvm_enable(void);
/* Disables interaction with external memory. */
void nvm_disable(void);
/* Resets the external memory device. */
void nvm_reset(void);
/* Begins a read session from external memory at a given address. */
void nvm_read(nvm_p address);
/* Obtains a byte from external memory during a read session. */
uint8_t nvm_get(void);
/* Catalogues a chunk of external memory with a given length and returns its pointer. */
nvm_p nvm_alloc(lf_size_t length);
/* Releases a previously catalogued chunk of external memory given its pointer. */
void nvm_free(nvm_p pointer);
/* Destructively resets all bits in external memory to 1s. */
void nvm_format(void);
/* Copies a block with source and length to the desitination internally. */
void nvm_copy(nvm_p destination, nvm_p source, lf_size_t length);
/* Moves a block of data with the source address and length given from internal memory to external memory. */
void nvm_push(void *source, lf_size_t length, nvm_p destination);
/* Moves a block of data with the source address and length given from external memory to internal memory. */
void nvm_pull(void *destination, lf_size_t length, nvm_p source);
/* Moves a block of data with the source address and length given from external memory to an internal memory buffer. */
void *nvm_dereference(nvm_p source, lf_size_t length);

/* ~ Declare the prototypes for the supporting functions belonging to this driver. ~ */
void nvm_transfer_page_to_buffer_with_erase(uint16_t page, uint8_t buffer);
void nvm_transfer_buffer_to_page_with_erase(uint8_t buffer, uint16_t page, uint8_t erase);
void nvm_begin_writing_to_buffer_with_offset(uint8_t buffer, uint16_t offset);
void nvm_begin_continuous_read(uint16_t page, uint16_t offset);

#endif
#endif
