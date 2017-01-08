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

#define _PSR 1
#define _PC 2
#define _LR 3

typedef enum {
    os_task_status_active,
    os_task_status_idle
} os_task_status;

struct _os_task {
    /* The task's stack pointer. */
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
    //printf("Application did finish.\n");
    /* Print reset message. */
    char fin_msg[] = "Application did finish.\n";
    usart_push(fin_msg, sizeof(fin_msg));

    /* Idle. */
    while(1) __NOP();
}

/* Initializes the scheduler. */
void os_task_init(void) {
        /* Configure the NVIC PendSV execption with the lowest possible priority. */
    NVIC_SetPriority(PendSV_IRQn, 0xff);

    /* Zero the schedule. */
    memset(&schedule, 0, sizeof(struct _os_schedule));

    /* Create the system task. */
    task_create(system_task, os_system_task_stack, SYSTEM_TASK_STACK_SIZE);
    /* Set the current task. */
    os_current_task = &schedule.tasks[SYSTEM_TASK];

    /* Set the PSP to the top of the system task's stack. */
    __set_PSP(os_current_task -> sp + 64);
    /* Switch to unprivilleged mode. */
    // __set_CONTROL(0x03);
    /* Flush the instruction pipeline. */
    __ISB();

    /* Exeucte the system task. */
    os_current_task -> handler();
}

/* Stages a task for launch. */
int task_create(void *handler, os_stack_t *stack, uint32_t stack_size) {
    /* If the OS is already scheduling the maximum number of tasks, fail. */
    if (schedule.count >= OS_MAX_TASKS) {
        return lf_error;
    }

    /* Obtain the next task slot. */
    struct _os_task *task = &schedule.tasks[schedule.count];
    /* Set the entry point of the task. */
    task -> handler = handler;
    /* Set the task's stack pointer, subtracting the 16 words needed to save the task's 16 registers. */
    task -> sp = (uint32_t)(stack + stack_size - 16);
    /* Start with the task idle. */
    task -> status = os_task_status_idle;

    /* Load default values into the task's special registers. */
    stack[stack_size - _PSR] = 0x01000000;
    stack[stack_size - _PC] = (uintptr_t)handler;
    stack[stack_size - _LR] = (uintptr_t)(&task_finished);

    /* Increment the number of active tasks. */
    schedule.count ++;

    return lf_success;
}

/* Schedules the next task for exeuction. */
void os_task_next(void) {
    /* Select the next task. */
    schedule.active ++;
    /* If we have reached the end of the task queue, loop back to the beginning. */
    if (schedule.active > schedule.count) {
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

    /* Register the task for launch. */
    task_create(application_entry, malloc(128 * sizeof(os_stack_t)), 128);
    /* Start the task. */
    os_task_next();

    return lf_success;
}
