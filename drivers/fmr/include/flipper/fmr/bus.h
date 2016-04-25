#ifndef __bus_h__
#define __bus_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

struct _bus {

	void (* const configure)(void *);
	void (* const enable)(void);
	void (* const disable)(void);
	uint8_t (* const ready)(void);
	void (* const put)(uint8_t byte);
	uint8_t (* const get)(void);
	void (* const push)(void *source, size_t length);
	void (* const pull)(void *destination, size_t length);

	uint8_t synchronous;

};

#endif
