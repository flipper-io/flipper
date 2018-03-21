/* scheduler.h - Primitive type definitions for the Osmium scheduler. */

#ifndef __scheduler_h__
#define __scheduler_h__

#include <flipper.h>
#include <os/loader.h>

/* An enumerated type of possible task states. */
typedef enum {
	os_task_status_unallocated,
	os_task_status_idle,
	os_task_status_active,
	os_task_status_paused
} os_task_status;

typedef uint32_t os_stack_t;

struct _os_task {
	/* The task's stack pointer. Points to the last item pushed onto the task's stack. */
	volatile uint32_t sp;
	/* The PID of this task. */
	int pid;
	/* The entry point of the task. */
	void (* handler)(void);
	/* The task's status (active or idle). */
	volatile os_task_status status;
	/* The base address of the task's stack, stored for task deallocation. */
	void *stack;
	/* The task's exit function. */
	void (* exit)(struct _lf_abi_header *header);
	/* The task's header. */
	struct _lf_abi_header *header;
	/* The next task to be executed. */
	struct _os_task *next;
};

struct _os_schedule {
	/* PID counter. */
	int next_pid;
	/* The system task's pointer. */
	struct _os_task *head;
	/* The active task PID. */
	volatile uint8_t active;
	/* The number of active tasks. */
	uint8_t count;
};

/* The PID of the system task. */
#define os_kernel_task_PID 0
/* System task stack size. */
#define KERNEL_TASK_STACK_SIZE_WORDS 128

/* Data structure to represent registers saved by the hardware. */
struct _stack_ctx {
	uint32_t r0;
	uint32_t r1;
	uint32_t r2;
	uint32_t r3;
	uint32_t r12;
	uint32_t lr;
	uint32_t pc;
	uint32_t psr;
};

/* Data structure to represent registers saved by the context switcher. */
struct _task_ctx {
	uint32_t r4;
	uint32_t r5;
	uint32_t r6;
	uint32_t r7;
	uint32_t r8;
	uint32_t r9;
	uint32_t r10;
	uint32_t r11;
};

void os_kernel_task(void);
void os_scheduler_init(void);

struct _os_task *os_task_create(void *_entry, void (* _exit)(struct _lf_abi_header *header), struct _lf_abi_header *header, uint32_t stack_size);
int os_task_add(struct _os_task *task);
int os_task_release(struct _os_task *task);
void os_task_next(void);

#endif
