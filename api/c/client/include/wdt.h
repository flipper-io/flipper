#ifndef __wdt_h__
#define __wdt_h__

/* Declare the prototypes for all of the functions within this module. */
int wdt_configure(struct _lf_device *device);
void wdt_fire(struct _lf_device *device);

#endif
