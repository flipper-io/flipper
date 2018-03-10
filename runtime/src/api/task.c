#include <flipper/task.h>

#ifdef __use_task__

LF_MODULE(_task, "task", "Pause, resume, or stop tasks running in the scheduler.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _task_interface task = {
	os_task_pause,
	os_task_resume,
	os_task_stop
};

LF_WEAK int os_task_pause(int pid) {
	return lf_invoke(lf_get_current_device(), &_task, _task_pause, lf_int_t, lf_args(lf_infer(pid)));
}

LF_WEAK int os_task_resume(int pid) {
	return lf_invoke(lf_get_current_device(), &_task, _task_resume, lf_int_t, lf_args(lf_infer(pid)));
}

LF_WEAK int os_task_stop(int pid) {
	return lf_invoke(lf_get_current_device(), &_task, _task_stop, lf_int_t, lf_args(lf_infer(pid)));
}

#endif
