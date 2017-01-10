/* Osmium scheduler implementation. */

#define __private_include__
#include <osmium.h>
#include <scheduler.h>

/* Pointers to the current and next tasks. */
volatile struct _os_task *volatile os_current_task;
volatile struct _os_task *volatile os_next_task;

/* Reserves the system task stack. */
os_stack_t os_system_task_stack[SYSTEM_TASK_STACK_SIZE_WORDS];

/* An empty schedule. */
struct _os_schedule schedule;

/* Called when an application finishes execution. */
void os_task_finished(void) {
    /* Release the task. */
    os_task_release(schedule.active);
    /* Context switch to the next task. */
    os_task_next();
    /* Prevent the CPU from wandering off. */
    while (1);
}

/* Initializes the scheduler. */
void os_task_init(void) {
    /* Configure the NVIC PendSV execption with the lowest possible priority. */
    NVIC_SetPriority(PendSV_IRQn, PENDSV_PRIORITY);
    /* Configure the NVIC SysTick exception with the highest possible priority. */
    NVIC_SetPriority(SysTick_IRQn, SYSTICK_PRIORITY);

    /* Clear the schedule. */
    memset(&schedule, 0, sizeof(struct _os_schedule));

    /* Create the system task. */
    os_task_create(system_task, os_system_task_stack, SYSTEM_TASK_STACK_SIZE_WORDS * sizeof(uint32_t));
    /* Make the current task the system task. */
    os_current_task = &schedule.tasks[SYSTEM_TASK_PID];

    /* Configure the SysTick to fire once every millisecond. */
    SysTick_Config(F_CPU / 1000);

    uint32_t psp = os_current_task -> sp + sizeof(struct _task_ctx) + sizeof(struct _stack_ctx);
    /* Set the PSP equal to the top of the system task's stack. */
    __set_PSP(psp);
    /* Switch processor mode to use PSP instead of MSP. */
    __set_CONTROL(0x02);
    /* Flush the instruction pipeline. This is required after a write to CONTROL. */
    __ISB();

    /* Exeucte the system task's handler. */
    os_current_task -> handler();

    /* Must hang here, otherwise the routine will pop values meant for MSP onto PSP. */
    while(1);
}

/* Stages a task for launch. */
struct _os_task *os_task_create(void *handler, os_stack_t *stack, uint32_t stack_size) {
    /* If the OS is already scheduling the maximum number of tasks, fail. */
    if (schedule.count >= OS_MAX_TASKS) {
        return NULL;
    }

    /* Search for an available_pid. */
    volatile int available_pid = -1;
    for (int i = 0; i < OS_MAX_TASKS; i ++) {
        if (!schedule.tasks[i].handler) {
            available_pid = i;
            break;
        }
    }

    /* If there is no available PID to satisfy the creation of a new task, fail. */
    if (available_pid == -1) {
        return NULL;
    }

    /* Allocate the next available task slot. */
    struct _os_task *task = &schedule.tasks[available_pid];
    /* Set the entry point of the task. */
    task -> handler = handler;
    /* Set the task's stack pointer to the top of the task's stack. */
    task -> sp = (uintptr_t)stack + stack_size;
    /* Mark the task as idle. */
    task -> status = os_task_status_idle;
    /* Set the base address of the task's stack. */
    task -> stack_base = stack;

    /* Push the stack context onto the process' stack. */
    task -> sp -= sizeof(struct _stack_ctx);

    struct _stack_ctx *_ctx = (struct _stack_ctx *)(task -> sp);
    /* Write the default value of the status register. */
    _ctx -> psr = 0x01000000;
    /* Write the handler address to the program counter. */
    _ctx -> pc = (uintptr_t)handler;
    /* Write the finished handler address into the link register. */
    _ctx -> lr = (uintptr_t)os_task_finished;

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

    return task;
}

/* Frees the memory associated with the task and frees its PID slot. */
int os_task_release(int pid) {
    /* Don't allow the user to release the system task. */
    if (pid == SYSTEM_TASK_PID) {
        return lf_error;
    }
    /* Get the task record. */
    struct _os_task *task = &schedule.tasks[pid];
    /* Disallow interrupts while freeing memory. */
    __disable_irq();
    /* If it was allocated, free the memory associated with the task's base. */
    if (task -> base) {
        free(task -> base);
    }
    /* If it was allocated, free the memory associated with the task's stack. */
    if (task -> stack_base) {
        free(task -> stack_base);
    }
    /* Allow interrupts again. */
    __enable_irq();
    /* Zero the task record. */
    memset(task, 0, sizeof(struct _os_task));
    /* Decrement the number of active tasks. */
    schedule.count --;
    return lf_success;
}

/* Schedules the next task for exeuction. */
void os_task_next(void) {
    /* Reload the pointer to the current task. */
    os_current_task = &schedule.tasks[schedule.active];
    /* Select the next task. */
    schedule.active ++;
    /* If we have reached the end of the task queue, loop back to the beginning. */
    if (schedule.active >= schedule.count) {
        schedule.active = 0;
    }
    /* Start the next task. */
    os_task_resume();
}

/* Pauses the execution of the current task. */
void os_task_pause(void) {

}

/* Resumes execution of the active task. */
void os_task_resume(void) {
    /* Mark the current task as idle. */
    os_current_task -> status = os_task_status_idle;
    /* Prepare to context switch to the active task. */
    os_next_task = &schedule.tasks[schedule.active];
    /* Mark the next task as active. */
    os_next_task -> status = os_task_status_active;
    /* Queue the PendSV exception to perform the context switch. */
	SCB -> ICSR |= SCB_ICSR_PENDSVSET_Msk;
}

/* Stop the execution of the active task and releases task memory. */
void os_task_stop(void) {

}

/* This function is called once per millisecond and triggers a context switch. */
void systick_exception(void) {
    /* Queue the execution of the next task. */
    os_task_next();
}
