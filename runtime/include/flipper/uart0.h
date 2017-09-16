#ifndef __uart0_h__
#define __uart0_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/libflipper.h>
#include <flipper/usart.h>

#ifdef __private_include__

struct _uart0_configuration {
	uint8_t b;
};

/* Declare the virtual interface for this modules. */
extern const struct _lf_endpoint uart0;

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _uart0;

/* Declare the prototypes for all of the functions within this module. */
extern int uart0_configure(struct _lf_endpoint *endpoint, void *_configuration);
extern bool uart0_ready(struct _lf_endpoint *endpoint);
extern int uart0_push(struct _lf_endpoint *endpoint, void *source, lf_size_t length);
extern int uart0_pull(struct _lf_endpoint *endpoint, void *destination, lf_size_t length);

extern void uart0_dfu(struct _lf_endpoint *endpoint);

#endif
#endif
