#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon/modules/task.h>

void os_task_pause(void) {
	lf_invoke(&_task, _task_pause, NULL);
}

void os_task_resume(void) {
	lf_invoke(&_task, _task_resume, NULL);
}

void os_task_stop(void) {
	lf_invoke(&_task, _task_stop, NULL);
}
