#ifndef __usart_h__
#define __usart_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Declare the virtual driver objects. ~ */
extern const struct _bus usart, usart1, dbgu;

#ifdef __private_include__

/* ~ ----------------------- USART0 ----------------------- ~ */

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _usart_configure, _usart_enable, _usart_disable, _usart_ready, _usart_put, _usart_get, _usart_push, _usart_pull };

/* ~ Declare all function prototypes for this driver. ~ */
void usart0_configure(void *baud);
void usart0_enable(void);
void usart0_disable(void);
uint8_t usart0_ready(void);
void usart0_put(uint8_t byte);
uint8_t usart0_get();
void usart0_push(void *source, uint32_t length);
void usart0_pull(void *destination, uint32_t length);

/* ~ ----------------------- USART1 ----------------------- ~ */

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _usart1_configure, _usart1_enable, _usart1_disable, _usart1_ready, _usart1_put, _usart1_get, _usart1_push, _usart1_pull };

/* ~ Declare all function prototypes for this driver. ~ */
 void usart1_configure(void *baud);
 void usart1_enable(void);
 void usart1_disable(void);
 uint8_t usart1_ready(void);
 void usart1_put(uint8_t byte);
 uint8_t usart1_get();
 void usart1_push(void *source, uint32_t length);
 void usart1_pull(void *destination, uint32_t length);

/* ~ ----------------------- DBGU ----------------------- ~ */

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _dbgu_configure, _dbgu_enable, _dbgu_disable, _dbgu_ready, _dbgu_put, _dbgu_get, _dbgu_push, _dbgu_pull };

/* ~ Declare all function prototypes for this driver. ~ */
void dbgu_configure(void *baud);
void dbgu_enable(void);
void dbgu_disable(void);
uint8_t dbgu_ready(void);
void dbgu_put(uint8_t byte);
uint8_t dbgu_get();
void dbgu_push(void *source, uint32_t length);
void dbgu_pull(void *destination, uint32_t length);

#endif
#endif
