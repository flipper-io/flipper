#ifndef __bus_h__

#define __bus_h__

#include <flipper/types.h>

struct _bus {
	
	void (* configure)(uint16_t configuration);
	
	void (* enable)(void);
	
	void (* disable)(void);
	
	bool (* ready)(void);
	
	void (* put)(uint8_t byte);
	
	uint8_t (* get)(void);
	
	void (* push)(void *source, uint32_t length);
	
	void (* pull)(void *destination, uint32_t length);
	
	bool synchronous;
	
};

#endif