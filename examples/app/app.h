#ifndef __my_app_h__
#define __my_app_h__

/* Include the platform header for the target platform. */
#include <flipper/carbon/platforms/atsam4s16b.h>

/* Put main in its own section. */
void main(void) __attribute__((section (".main")));

#endif
