#define __private_include__

#include <usart/usart.h>

/* ~----------------------- USART0 -----------------------~ */

const struct _bus usart = {
	
	usart0_configure,
	
	usart0_enable,
	
	usart0_disable,
	
	usart0_ready,
	
	usart0_put,
	
	usart0_get,
	
	usart0_push,
	
	usart0_pull,
	
	false
	
};

/* ~----------------------- USART1 -----------------------~ */

const struct _bus usart1 = {
	
	usart1_configure,
	
	usart1_enable,
	
	usart1_disable,
	
	usart1_ready,
	
	usart1_put,
	
	usart1_get,
	
	usart1_push,
	
	usart1_pull,
	
	false
	
};

/* ~----------------------- DBGU -----------------------~ */

const struct _bus dbgu = {
	
	dbgu_configure,
	
	dbgu_enable,
	
	dbgu_disable,
	
	dbgu_ready,
	
	dbgu_put,
	
	dbgu_get,
	
	dbgu_push,
	
	dbgu_pull,
	
	false
	
};