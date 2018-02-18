#ifndef __task_h__
#define __task_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the virtual interface for this module. */
extern const struct _task {
	/* Pasues the running task. */
	int (* pause)(int pid);
	/* Resumes the running task. */
	int (* resume)(int pid);
	/* Stops the running task. */
	int (* stop)(int pid);
} task;

#ifdef __private_include__

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _task;

/* Declare the FMR overlay for this module. */
enum { _task_pause, _task_resume, _task_stop };

int os_task_pause(int pid);
int os_task_resume(int pid);
int os_task_stop(int pid);

#endif
#endif
