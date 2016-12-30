#ifndef __usart_h__
#define __usart_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this modules. */
extern const struct _uart {
	int (* configure)(void);
	void (* enable)(void);
	void (* disable)(void);
	uint8_t (* ready)(void);
	void (* put)(uint8_t byte);
	uint8_t (* get)(void);
	int (* push)(void *source, lf_size_t length);
	int (* pull)(void *destination, lf_size_t length);
} usart;

#ifdef __private_include__

/* The fmr_module structure for this module. */
extern struct _lf_module _usart;

/* Declare the FMR overlay for this driver. */
enum { _usart_configure, _usart_enable, _usart_disable, _usart_ready, _usart_put, _usart_get, _usart_push, _usart_pull };

/* Declare each prototype for all functions within this driver. */
int usart_configure(void);
void usart_enable(void);
void usart_disable(void);
uint8_t usart_ready(void);
void usart_put(uint8_t byte);
uint8_t usart_get();
int usart_push(void *source, lf_size_t length);
int usart_pull(void *destination, lf_size_t length);

#endif
#endif
