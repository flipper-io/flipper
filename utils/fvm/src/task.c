#include <flipper.h>

#ifdef __use_task__
#include <flipper/task.h>

int os_task_pause(int pid) {
	printf("Pausing the task with pid %i.\n", pid);
	return lf_success;
}

int os_task_resume(int pid) {
	printf("Resuming the task with pid %i.\n", pid);
	return lf_success;
}

int os_task_stop(int pid) {
	printf("Stopping the task with pid %i.\n", pid);
	return lf_success;
}

#endif
