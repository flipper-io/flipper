#ifndef error_h

#define error_h

/*
 
 Driver: error
 
 This driver oversees error handling across the heirarchy of targets connected to the active instance of the toolbox.
 
 */

#include <flipper/types.h>

/* ~ Define the virtual interface for this driver. ~ */

extern const struct _error {
	
	void (* configure)(void);
	
	/* ~ Throws an error across the entire heirarchy. ~ */
	
	void (* raise)(uint16_t id);
	
	/* ~ Gets the message for the current error. ~ */
	
	char *(* message)(void);
	
	/* ~ The current error message. ~ */
	
	uint16_t error;
	
} error;

#ifdef __private_include__

/* ~ Define the overlay for this driver. ~ */

enum { _error_configure, _error_throw };

/* ~ Explicitly reference the functions needed by this driver. ~ */

extern void error_configure(void);

extern void error_raise(uint16_t id);

extern char *error_message(void);

#endif


#endif