#include <flipper/flipper.h>

/* ~ Dynamically loaded object references. ~ */

struct _fdl __attribute__((section(".fdl"))) *_fdl;

const struct _io io;

#define IO_KEY 0xf01d

/* ~ Program initialization function. ~ */

void __init() __attribute__((section(".init")));

void __init() {
	
	//_fdl -> configure();
	
	//_fdl -> resolve(IO_KEY, &io);
	
	/* ~ Call the main function. ~ */
	
	main();
	
}