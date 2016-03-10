#define __private_include__
#include <flipper/core.h>
#include <platform.h>

void (* task_to_execute)(void);

#pragma GCC push_options

#pragma GCC optimize ("O0")

void begin_scheduling(void) {
	
	/* ~ Rudimentary scheduling system. Check to see if we have a recently loaded task to execute; if we do, execute it. ~ */
	
	while (true) {
		
		/* ~ Ensure the function pointer lies within valid executable address space. ~ */
		
		if ((void *)(task_to_execute) > (void *)(AT91C_IFLASH) && (void *)(task_to_execute) < (void *)(AT91C_IFLASH + AT91C_IFLASH_SIZE)) { task_to_execute(); }
		
	}
	
}

#pragma GCC pop_options