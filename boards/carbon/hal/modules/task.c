#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon/modules/task.h>

int os_task_pause(int pid) {
	return lf_invoke(&_task, _task_pause, fmr_args(fmr_infer(pid)));
}

int os_task_resume(int pid) {
	return lf_invoke(&_task, _task_resume, fmr_args(fmr_infer(pid)));
}

int os_task_stop(int pid) {
	return lf_invoke(&_task, _task_stop, fmr_args(fmr_infer(pid)));
}
