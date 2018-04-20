#ifndef __uart0_h__
#define __uart0_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>
#include <flipper/usart.h>

/* Declare the virtual interface for this modules. */
extern const struct _uart0_interface {
	int (* configure)(uint8_t baud, uint8_t interrupts);
	int (* ready)(void);
	int (* push)(void *source, lf_size_t length);
	int (* pull)(void *destination, lf_size_t length);
} uart0;

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _uart0;

/* Declare the FMR overlay for this module. */
enum { _uart0_configure, _uart0_ready, _uart0_push, _uart0_pull };

/* Declare the prototypes for all of the functions within this module. */
int uart0_configure(uint8_t baud, uint8_t interrupts);
int uart0_ready(void);
int uart0_push(void *source, lf_size_t length);
int uart0_pull(void *destination, lf_size_t length);

#endif
