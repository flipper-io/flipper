#ifndef __uart_h__
#define __uart_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this modules. */
extern const struct _uart {
	void (*configure)();
	void (* enable)(void);
	void (* disable)(void);
	uint8_t (* ready)(void);
	void (* put)(uint8_t byte);
	uint8_t (* get)(void);
	void (* push)(void *source, uint32_t length);
	void (* pull)(void *destination, uint32_t length);
} uart;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _uart_configure, _uart_enable, _uart_disable, _uart_ready, _uart_put, _uart_get, _uart_push, _uart_pull };

/* Declare each prototype for all functions within this driver. */
void uart_configure(void);
void uart_enable(void);
void uart_disable(void);
uint8_t uart_ready(void);
void uart_put(uint8_t byte);
uint8_t uart_get();
void uart_push(void *source, uint32_t length);
void uart_pull(void *destination, uint32_t length);

#endif
#endif
