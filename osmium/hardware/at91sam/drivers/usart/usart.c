#define __private_include__

#include <drivers/usart.h>

/* ------------------------ USART0 ------------------------ */

const struct _bus usart = {
	
	usart0_configure,
	
	usart0_enable,
	
	usart0_disable,
	
	usart0_ready,
	
	usart0_put_byte,
	
	usart0_get_byte,
	
	usart0_push,
	
	usart0_pull
	
};

/* ------------------------ USART1 ------------------------ */

const struct _bus usart1 = {
	
	usart1_configure,
	
	usart1_enable,
	
	usart1_disable,
	
	usart1_ready,
	
	usart1_put_byte,
	
	usart1_get_byte,
	
	usart1_push,
	
	usart1_pull
	
};

/* ------------------------ DBGU ------------------------ */

const struct _bus dbgu = {
	
	dbgu_configure,
	
	dbgu_enable,
	
	dbgu_disable,
	
	dbgu_ready,
	
	dbgu_put_byte,
	
	dbgu_get_byte,
	
	dbgu_push,
	
	dbgu_pull
	
};