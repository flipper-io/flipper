#ifndef __timer_h__
#define __timer_h__

/* Declare the prototypes for all of the functions within this module. */
int timer_configure(struct _lf_device *device);
int timer_register(struct _lf_device *device, uint32_t ticks, void *callback);

#endif
