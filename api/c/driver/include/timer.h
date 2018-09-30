#ifndef __timer_h__
#define __timer_h__

/* Declare the prototypes for all of the functions within this module. */
int timer_configure(void);
int timer_register(uint32_t ticks, void *callback);

#endif
