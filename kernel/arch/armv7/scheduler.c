/* Osmium scheduler implementation. */

#include <flipper.h>
#include <os/scheduler.h>

/* Pointers to the current and next tasks. */
struct _os_task *os_current_task;
struct _os_task *os_next_task;

/* Reserves the system task stack. */
os_stack_t kernel_task_stack[KERNEL_TASK_STACK_SIZE_WORDS];

/* An empty schedule. */
struct _os_schedule schedule;

/* Called when an application finishes execution. */
void os_task_finished(void) {
	/* Release the current task. */
	os_task_release(os_current_task);
	/* Prevent the CPU from wandering off. */
	while (1);
}

/* Initializes the scheduler. */
void os_scheduler_init(void) {
	/* Configure the NVIC PendSV execption with the lowest possible priority. */
	NVIC_SetPriority(PendSV_IRQn, PENDSV_PRIORITY);
	/* Configure the NVIC SysTick exception with the highest possible priority. */
	NVIC_SetPriority(SysTick_IRQn, SYSTICK_PRIORITY);

	/* Clear the schedule. */
	memset(&schedule, 0, sizeof(struct _os_schedule));

	/* Create the system task. */
	struct _os_task *task = os_task_create(os_kernel_task, NULL, NULL, KERNEL_TASK_STACK_SIZE_WORDS * sizeof(uint32_t));
	/* Add the task. */
	os_task_add(task);
	/* Make the current task and the head of the task list the system task. */
	os_current_task = schedule.head = task;
	/* Make the system task's next task point back to the system task. */
	task->next = task;

	/* Configure the SysTick to fire once every millisecond. */
	SysTick_Config(F_CPU / 1000);

	uint32_t psp = task->sp + sizeof(struct _task_ctx) + sizeof(struct _stack_ctx);
	/* Set the PSP equal to the top of the system task's stack. */
	__set_PSP(psp);
	/* Switch processor mode to use PSP instead of MSP. */
	__set_CONTROL(0x02);
	/* Flush the instruction pipeline. This is required after a write to CONTROL. */
	__ISB();

	/* Execute the system task's handler. */
	task->handler();

	/* Must hang here, otherwise this routine will pop values meant for MSP onto PSP. */
	while (1) __asm__ __volatile__ ("nop");
}

int os_task_add(struct _os_task *task) {

	if (schedule.count) {
		/* Walk to the end of the task list. */
		struct _os_task *_tail = schedule.head;
		int count = schedule.count;
		while (-- count) _tail = _tail->next;
		/* Set the tail of the task list's next task equal to the newly allocated task. */
		_tail->next = task;
	}

	/* Increment the number of active tasks. */
	schedule.count ++;

	return lf_success;
}

struct _os_task *os_task_create(void *_entry, void (* _exit)(struct _lf_abi_header *header), struct _lf_abi_header *header, uint32_t stack_size) {
	/* Allocate the next available task slot. */
	struct _os_task *task = malloc(sizeof(struct _os_task));
	lf_assert(task, failure, E_NULL, "Failed to allocate memory to create task");
	os_stack_t *stack = malloc(stack_size);
	lf_assert(stack, failure, E_NULL, "Failed to allocate memory to create stack.");

	/* Set the task's stack pointer to the top of the task's stack. */
	task->sp = (uintptr_t)stack + stack_size;
	/* Set the PID of the task. */
	task->pid = schedule.next_pid ++;
	/* Set the entry point of the task. */
	task->handler = _entry;
	/* Mark the task as idle. */
	task->status = os_task_status_idle;
	/* Store the address of the task's stack. */
	task->stack = stack;
	/* Set the task's exit function. */
	task->exit = _exit;
	/* Set the task's exit context. */
	task->header = header;
	/* Set the task's next task equal to the head of the task list. */
	task->next = schedule.head;

	/* Push the stack context onto the process' stack. */
	task->sp -= sizeof(struct _stack_ctx);

	struct _stack_ctx *_stack = (struct _stack_ctx *)(task->sp);
	/* Write the default value of the status register. */
	_stack->psr = 0x01000000;
	/* Write the entry point address to the program counter. */
	_stack->pc = (uintptr_t)_entry;
	/* Write the exit handler address into the link register. */
	_stack->lr = (uintptr_t)os_task_finished;

	/* Push the stack context onto the process stack. */
	task->sp -= sizeof(struct _task_ctx);

	struct _task_ctx *_tsk = (struct _task_ctx *)(task->sp);
	_tsk->r4 = 4;
	_tsk->r5 = 5;
	_tsk->r6 = 6;
	_tsk->r7 = 7;
	_tsk->r8 = 8;
	_tsk->r9 = 9;
	_tsk->r10 = 10;
	_tsk->r11 = 11;

	return task;
failure:
	return NULL;
}

int os_task_release(struct _os_task *task) {
	lf_assert(task, failure, E_NULL, "Invalid task pointer provided to '%s'.", __PRETTY_FUNCTION__);
	lf_assert(task != schedule.head, failure, E_INVALID_TASK, "Tried to release task head.");
	/* Disallow interrupts while freeing memory. */
	__disable_irq();
	/* Call the task's exit function. */
	if (task->exit) task->exit(task->header);
	/* If it was allocated, free the memory associated with the task's stack. */
	if (task->stack) free(task->stack);
	/* Allow interrupts again. */
	__enable_irq();
	/* Update the parent task's next pointer. */
	struct _os_task *parent = schedule.head;
	for (int i = 0; i < schedule.count; i ++) {
		if (parent->next == task) {
			parent->next = task->next;
			break;
		}
	}
	/* If the released task was the actively executing task, move on to the next task. */
	if (task == os_current_task) {
		os_current_task = NULL;
		os_next_task = task->next;
		os_task_next();
	}
	/* Free the task record. */
	free(task);
	/* Decrement the number of active tasks. */
	schedule.count --;
	return lf_success;
failure:
	return lf_error;
}

/* Schedules the next task for exeuction. */
void os_task_next(void) {
	if (os_current_task) {
		/* Mark the current task as idle. */
		os_current_task->status = os_task_status_idle;
		/* Prepare to context switch to the active task. */
		os_next_task = os_current_task->next;
	}
check:
	/* If the next task is paused, skip it. */
	if (os_next_task->status == os_task_status_paused) {
		os_next_task = os_next_task->next;
		/* Check if the next next task was paused, so on and so fourth. */
		goto check;
	}
	/* Mark the next task as active. */
	os_next_task->status = os_task_status_active;
	/* Queue the PendSV exception to perform the context switch. */
	SCB->ICSR |= SCB_ICSR_PENDSVSET_Msk;
}

/* Called at the end of the PendSV exception to cycle the task pointers. */
void os_update_task_pointers(void) {
	/* Make the current task the next task. */
	os_current_task = os_next_task;
}

/* Gets the task pointer for a given PID. */
struct _os_task *os_task_from_pid(int pid) {
	struct _os_task *task = schedule.head;
	for (int i = 0; i < schedule.count; i ++) {
		if (pid == task->pid) {
			return task;
		}
		task = task->next;
	}
	return NULL;
}

/* Pauses the execution of the current task. */
int os_task_pause(int pid) {
	/* Circumvent users from interacting with the system task. */
	if (!pid) {
		lf_error_raise(E_INVALID_TASK, NULL);
		return lf_error;
	}
	/* Find the task for the given PID. */
	struct _os_task *task = os_task_from_pid(pid);
	if (!task) {
		lf_error_raise(E_NO_PID, NULL);
		return lf_error;
	}
	/* If the task is the currently executing task, queue the move to the next task. */
	if (os_current_task == task) {
		os_task_next();
	}
	/* Mark the task as paused. */
	task->status = os_task_status_paused;
	return lf_success;
}

/* Resumes execution of a given task. */
int os_task_resume(int pid) {
	/* Circumvent users from interacting with the system task. */
	if (!pid) {
		return lf_success;
	}
	/* Find the task for the given PID. */
	struct _os_task *task = os_task_from_pid(pid);
	if (!task) {
		lf_error_raise(E_NO_PID, NULL);
		return lf_error;
	}
	/* Mark the task as idle so it is executed during the next scheduling event. */
	task->status = os_task_status_idle;
	/* Execute the resumed task next. */
	os_current_task->status = os_task_status_idle;
	os_current_task = NULL;
	os_next_task = task;
	os_task_next();
	return lf_success;
}

/* Stop the execution of the active task and releases task memory. */
int os_task_stop(int pid) {
	/* Circumvent users from interacting with the system task. */
	if (!pid) {
		lf_error_raise(E_INVALID_TASK, NULL);
		return lf_error;
	}
	/* Find the task for the given PID. */
	struct _os_task *task = os_task_from_pid(pid);
	/* Release the task with the given PID. */
	return os_task_release(task);
}

/* This function is called once per millisecond and triggers a context switch. */
void systick_exception(void) {
	/* Queue the execution of the next task. */
	os_task_next();
}
