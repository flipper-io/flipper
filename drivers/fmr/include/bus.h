#ifndef __bus_h__
#define __bus_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/flipper/core.h>

struct _bus {

	void (* configure)(void *);
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
