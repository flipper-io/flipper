#ifndef __task_h__
#define __task_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

int os_task_pause(int pid);
int os_task_resume(int pid);
int os_task_stop(int pid);

#endif
