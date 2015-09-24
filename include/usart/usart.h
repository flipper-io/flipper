#ifndef __usart_h__

#define __usart_h__

#include <types.h>

#include <fmr/bus.h>

extern const struct _bus usart, usart1, dbgu;

#ifdef __private_include__

enum { _usart_configure, _usart_enable, _usart_disable, _usart_ready, _usart_put, _usart_get, _usart_push, _usart_pull };

extern void usart0_configure(uint16_t baud);

extern void usart0_enable(void);

extern void usart0_disable(void);

extern bool usart0_ready(void);

extern void usart0_put(uint8_t byte);

extern uint8_t usart0_get();

extern void usart0_push(void *source, uint32_t length);

extern void usart0_pull(void *destination, uint32_t length);


enum { _usart1_configure, _usart1_enable, _usart1_disable, _usart1_ready, _usart1_put, _usart1_get, _usart1_push, _usart1_pull };

extern void usart1_configure(uint16_t baud);

extern void usart1_enable(void);

extern void usart1_disable(void);

extern bool usart1_ready(void);

extern void usart1_put(uint8_t byte);

extern uint8_t usart1_get();

extern void usart1_push(void *source, uint32_t length);

extern void usart1_pull(void *destination, uint32_t length);


enum { _dbgu_configure, _dbgu_enable, _dbgu_disable, _dbgu_ready, _dbgu_put, _dbgu_get, _dbgu_push, _dbgu_pull };

extern void dbgu_configure(uint16_t baud);

extern void dbgu_enable(void);

extern void dbgu_disable(void);

extern bool dbgu_ready(void);

extern void dbgu_put(uint8_t byte);

extern uint8_t dbgu_get();

extern void dbgu_push(void *source, uint32_t length);

extern void dbgu_pull(void *destination, uint32_t length);

#endif

#endif