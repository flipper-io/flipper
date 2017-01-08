#define __private_include__
#include <flipper/carbon/modules/task.h>

#ifdef __use_task__
/* Define the virtual interface for this module. */
const struct _task task = {
	os_task_pause,
	os_task_resume,
	os_task_stop
};
#endif
