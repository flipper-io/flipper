/* Osmium loader implementation. */

#define __private_include__
#include <osmium.h>

/*
 * The loader handles the loading of moudles and applications into program memory
 * and RAM for debugging. Flipper moudles and applications must be compiled
 * to comply with the Flipper ABI (Application Binary Interface) such that
 * the loader can properly handle the relocation of the data segment for modules
 * and applications loaded into ROM. RAM loaded modules and applications do not
 * require relocation of their .data and .bss sections, and thus be compiled as
 * position independant, and without a GOT (Global Offset Table).
 *
 */


 /* FDL Module/Application ABI Specification */

 /*------------------------------+  0x0000     --+
  |          entry point         |               |
  +------------------------------+  0x0004       |
  |    sizeof(struct _module)    |               |
  +------------------------------+  0x0008       |
  |      &(struct _module)       |               |
  +------------------------------+  0x000c       | - HEADER
  |          .data size          |               |
  +------------------------------+  0x0010       |
  |         .data offset         |               |
  +------------------------------+  0x0014       |
  |          .bss size           |               |
  +------------------------------+  0x0018     --+
  |         .bss offset          |
  +------------------------------+  0x0018
  |            .got              |
  +------------------------------+  0x0010 + [0x0000]
  |            .text             |
  +------------------------------+  0x0010 + [0x000c]
  |            .data             |
  +------------------------------+  0x0010 + [0x0014]
  |             .bss             |
  +------------------------------*/

/* The data structure definition representing the ABI header above. */
struct _fld_header {
    uint32_t entry;
    uint32_t module_size;
    uint32_t module_offset;
    uint32_t data_size;
    uint32_t data_offset;
    uint32_t bss_size;
    uint32_t bss_offset;
    uint32_t got_size;
    uint32_t got_offset;
};

#define OS_MAX_TASKS 3

typedef enum {
    os_task_status_active,
    os_task_status_idle
} os_task_status;

struct _os_task {
    /* The task's stack pointer. Points to the last item pushed onto the task's stack. */
    volatile uint32_t sp;
    /* The entry point of the task. */
    void (* handler)(void);
    /* The task's status (active or idle). */
    volatile os_task_status status;
};

struct _os_schedule {
    /* A table holding pointers to the active tasks. */
    struct _os_task tasks[OS_MAX_TASKS];
    /* The active task PID. */
    volatile uint8_t active;
    /* The number of active tasks. */
    uint8_t count;
} schedule;

#define SYSTEM_TASK 0
/* System task stack size. */
#define SYSTEM_TASK_STACK_SIZE 128
/* System task stack. */
os_stack_t os_system_task_stack[SYSTEM_TASK_STACK_SIZE];

/* Pointers to the current and next tasks. */
struct _os_task *volatile os_current_task;
struct _os_task *volatile os_next_task;

/* Called when an application finishes execution. */
void task_finished(void) {
    /* Debug message. */
    printf("Application finished executing. Hanging its PC.\n");
    /* Idle. */
    while(1) __NOP();
}

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

/* Initializes the scheduler. */
void os_task_init(void) {
    /* Configure the NVIC PendSV execption with the lowest possible priority. */
    NVIC_SetPriority(PendSV_IRQn, PENDSV_PRIORITY);
    //NVIC_SetPriority(SysTick_IRQn, SYSTICK_PRIORITY);

    /* Zero the schedule. */
    memset(&schedule, 0, sizeof(struct _os_schedule));

    /* Create the system task. */
    task_create(system_task, os_system_task_stack, SYSTEM_TASK_STACK_SIZE);
    /* Set the current task. */
    os_current_task = &schedule.tasks[SYSTEM_TASK];

    /* Start the systick. */
    //SysTick_Config(F_CPU);

    /* Set the PSP equal to the system task's stack. */
    uint32_t psp = os_current_task -> sp + sizeof(struct _task_ctx) + sizeof(struct _stack_ctx);
    __set_PSP(psp);
    /* Switch to using PSP. */
    __set_CONTROL(0x02);
    /* Flush the instruction pipeline. */
    __ISB();

    /* Exeucte the system task. */
    os_current_task -> handler();

    /* Must hang here, otherwise the routine will pop values meant for MSP onto PSP. */
    while(1);
}

/* Stages a task for launch. */
int task_create(void *handler, os_stack_t *stack, uint32_t stack_size) {
    /* If the OS is already scheduling the maximum number of tasks, fail. */
    if (schedule.count >= OS_MAX_TASKS) {
        return lf_error;
    }

    /* Obtain the next task slot. */
    struct _os_task *task = &schedule.tasks[schedule.count];
    printf("Filling task slot %d.\n", schedule.count);
    /* Set the entry point of the task. */
    task -> handler = handler;
    /* Set the stack pointer. */
    task -> sp = (uintptr_t)stack + stack_size;
    printf("Created task with user stack pointer %p.\n", task -> sp);
    /* Start with the task idle. */
    task -> status = os_task_status_idle;

    /* Load default values into the task's special registers. */
    /* Push the stack context onto the process stack. */
    task -> sp -= sizeof(struct _stack_ctx);
    struct _stack_ctx *_ctx = (struct _stack_ctx *)(task -> sp);
    /* This is the default value of the status register. */
    _ctx -> psr = 0x01000000;
    /* This is the address at which our task will begin executing. */
    _ctx -> pc = (uintptr_t)handler;
    /* This is the address that the task will jump to when it is finished. */
    _ctx -> lr = (uintptr_t)task_finished;

    /* For debugging initial task state. */
    /* Push the stack context onto the process stack. */
    task -> sp -= sizeof(struct _task_ctx);
    struct _task_ctx *_tsk = (struct _task_ctx *)(task -> sp);
    _tsk -> r4 = 4;
    _tsk -> r5 = 5;
    _tsk -> r6 = 6;
    _tsk -> r7 = 7;
    _tsk -> r8 = 8;
    _tsk -> r9 = 9;
    _tsk -> r10 = 10;
    _tsk -> r11 = 11;

    /* Increment the number of active tasks. */
    schedule.count ++;

    return lf_success;
}

/* Schedules the next task for exeuction. */
void os_task_next(void) {
    os_current_task = &schedule.tasks[schedule.active];
    /* Select the next task. */
    schedule.active ++;
    /* If we have reached the end of the task queue, loop back to the beginning. */
    if (schedule.active >= schedule.count) {
        schedule.active = 0;
    }
    /* Start the task. */
    os_task_resume();
}

/* Pauses the execution of the current task. */
void os_task_pause(void) {
    os_task_next();
}

/* Resume execution of the active task. */
void os_task_resume(void) {
    printf("Switching execution to task slot %d.\n", schedule.active);
    /* Set the current task as idle. */
    os_current_task -> status = os_task_status_idle;
    /* Make the next task the system task. */
    os_next_task = &schedule.tasks[schedule.active];
    /* Set the system task as active. */
    os_next_task -> status = os_task_status_active;
    /* Trigger the PendSV exception to perform the context switch. */
	SCB -> ICSR |= SCB_ICSR_PENDSVSET_Msk;
}

/* Stop the execution of the active task and release the task memory. */
void os_task_stop(void) {

}

/* ------------------------------------ */

/* Loads a module or application located at the given address. */
int os_load(void *address) {
    /* Cast to the ABI header. */
    struct _fld_header *header = address;

    /* Patch the GOT. */
    uintptr_t *got = (uintptr_t *)(address + header->got_offset);
    for (int i = 0; i < header->got_size / sizeof(uint32_t); i ++) {
        got[i] += (uintptr_t)address;
    }
    /* Set the entry point of the image. */
    void *application_entry = address + header->entry + 1;

    printf("Loaded application at address %p.\n", address);
    printf("Application entry is at address %p.\n", application_entry);

    /* Register the task for launch. */
    task_create(application_entry, malloc(256), 256);
    /* Start the task. */
    os_task_next();

    printf("Done creating task.\n");

    return lf_success;
}

void systick_exception(void) {
    os_task_next();
}
