#include <flipper/flipper.h>

/* ~ Dynamically loaded object references. ~ */

__attribute__((section(".fdl"))) const struct _fdl *_fdl;

__attribute__((section(".modules"))) const struct _io io;

#define IO_KEY 0xf01d

/* ~ Program initialization function. ~ */

__attribute__((section(".init"))) void __init() {
	
	_fdl -> resolve(IO_KEY, &io);
	
	/* ~ Call the main function. ~ */
	
	main();
	
}