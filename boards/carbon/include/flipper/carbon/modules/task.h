#ifndef __task_h__
#define __task_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/libflipper.h>

/* Declare the virtual interface for this module. */
extern const struct _task {
	/* Pasues the running task. */
	void (* pause)(void);
	/* Resumes the running task. */
	void (* resume)(void);
	/* Stops the running task. */
	void (* stop)(void);
} task;

#ifdef __private_include__

/* The fmr_module structure for this module. */
extern struct _lf_module _task;

/* Declare the FMR overlay for this driver. */
enum { _task_pause, _task_resume, _task_stop };

extern void os_task_pause(void);
extern void os_task_resume(void);
extern void os_task_stop(void);

#endif
#endif
