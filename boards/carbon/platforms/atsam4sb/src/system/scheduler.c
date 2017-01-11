/* Osmium scheduler implementation. */

#define __private_include__
#include <osmium.h>
#include <scheduler.h>

/* Pointers to the current and next tasks. */
struct _os_task *os_current_task;
struct _os_task *os_next_task;

/* Reserves the system task stack. */
os_stack_t os_system_task_stack[SYSTEM_TASK_STACK_SIZE_WORDS];

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
void os_task_init(void) {
    /* Configure the NVIC PendSV execption with the lowest possible priority. */
    NVIC_SetPriority(PendSV_IRQn, PENDSV_PRIORITY);
    /* Configure the NVIC SysTick exception with the highest possible priority. */
    NVIC_SetPriority(SysTick_IRQn, SYSTICK_PRIORITY);

    /* Clear the schedule. */
    memset(&schedule, 0, sizeof(struct _os_schedule));

    /* Create the system task. */
    struct _os_task *task = os_task_create(system_task, os_system_task_stack, SYSTEM_TASK_STACK_SIZE_WORDS * sizeof(uint32_t));
    /* Make the current task and the head of the task list the system task. */
    os_current_task = schedule.head = task;
    /* Make the system task's next task point back to the system task. */
    task -> next = task;

    /* Configure the SysTick to fire once every millisecond. */
    SysTick_Config(F_CPU / 1000);

    uint32_t psp = task -> sp + sizeof(struct _task_ctx) + sizeof(struct _stack_ctx);
    /* Set the PSP equal to the top of the system task's stack. */
    __set_PSP(psp);
    /* Switch processor mode to use PSP instead of MSP. */
    __set_CONTROL(0x02);
    /* Flush the instruction pipeline. This is required after a write to CONTROL. */
    __ISB();

    /* Exeucte the system task's handler. */
    task -> handler();

    /* Must hang here, otherwise the routine will pop values meant for MSP onto PSP. */
    while (1);
}

/* Stages a task for launch. */
struct _os_task *os_task_create(void *handler, os_stack_t *stack, uint32_t stack_size) {
    /* If the OS is already scheduling the maximum number of tasks, fail. */
    if (schedule.count >= OS_MAX_TASKS) {
        return NULL;
    }

    /* Allocate the next available task slot. */
    struct _os_task *task = malloc(sizeof(struct _os_task));
    /* Ensure memory was allocated for the new task. */
    if (!task) {
        lf_error_raise(E_MALLOC, NULL);
        return NULL;
    }

    if (schedule.count) {
        /* Walk to the end of the task list. */
        struct _os_task *_tail = schedule.head;
        int count = schedule.count;
        while (-- count) _tail = _tail -> next;
        /* Set the tail of the task list's next task equal to the newly allocated task. */
        _tail -> next = task;
    }

    /* Set the task's stack pointer to the top of the task's stack. */
    task -> sp = (uintptr_t)stack + stack_size;
    /* Set the PID of the task. */
    task -> pid = schedule.next_pid ++;
    /* Set the entry point of the task. */
    task -> handler = handler;
    /* Mark the task as idle. */
    task -> status = os_task_status_idle;
    /* Set the base address of the task's stack. */
    task -> stack_base = stack;
    /* Set the task's next task equal to the head of the task list. */
    task -> next = schedule.head;

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
int os_task_release(struct _os_task *task) {
    if (!task) {
        lf_error_raise(E_NULL, NULL);
        return lf_error;
    }
    /* Don't allow the user to release the system task. */
    if (task == schedule.head) {
        lf_error_raise(E_UNIMPLEMENTED, NULL);
        return lf_error;
    }
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
    /* Update the parent task's next pointer. */
    struct _os_task *parent = schedule.head;
    for (int i = 0; i < schedule.count; i ++) {
        if (parent -> next == task) {
            parent -> next = task -> next;
            break;
        }
    }
    /* If the released task was the actively executing task, move on to the next task. */
    if (task == os_current_task) {
        os_current_task = NULL;
        os_next_task = task -> next;
        os_task_next();
    }
    /* Free the task record. */
    free(task);
    /* Decrement the number of active tasks. */
    schedule.count --;
    return lf_success;
}

/* Schedules the next task for exeuction. */
void os_task_next(void) {
    if (os_current_task) {
        /* Mark the current task as idle. */
        os_current_task -> status = os_task_status_idle;
        /* Prepare to context switch to the active task. */
        os_next_task = os_current_task -> next;
    }
check:
    /* If the next task is paused, skip it. */
    if (os_next_task -> status == os_task_status_paused) {
        os_next_task = os_next_task -> next;
        /* Check if the next next task was paused, so on and so fourth. */
        goto check;
    }
    /* Mark the next task as active. */
    os_next_task -> status = os_task_status_active;
    /* Queue the PendSV exception to perform the context switch. */
    SCB -> ICSR |= SCB_ICSR_PENDSVSET_Msk;
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
        if (pid == task -> pid) {
            return task;
        }
        task = task -> next;
    }
    return NULL;
}

/* Pauses the execution of the current task. */
int os_task_pause(int pid) {
    /* Circumvent users from interacting with the system task. */
    if (!pid) {
        return lf_error;
    }
    /* Find the task for the given PID. */
    struct _os_task *task = os_task_from_pid(pid);
    /* Ensure that the PID was valid. */
    if (!task) {
        return lf_error;
    }
    /* If the task is the currently executing task, queue the move to the next task. */
    if (os_current_task == task) {
        os_task_next();
    }
    /* Mark the task as paused. */
    task -> status = os_task_status_paused;
    return lf_success;
}

/* Resumes execution of a given task. */
int os_task_resume(int pid) {
    /* Circumvent users from interacting with the system task. */
    if (!pid) {
        return lf_error;
    }
    /* Find the task for the given PID. */
    struct _os_task *task = os_task_from_pid(pid);
    /* Ensure that the PID was valid. */
    if (!task) {
        return lf_error;
    }
    /* Mark the task as idle so it is executed during the next scheduling event. */
    task -> status = os_task_status_idle;
    /* Execute the resumed task next. */
    os_current_task -> status = os_task_status_idle;
    os_current_task = NULL;
    os_next_task = task;
    os_task_next();
    return lf_success;
}

/* Stop the execution of the active task and releases task memory. */
int os_task_stop(int pid) {
    /* Circumvent users from interacting with the system task. */
    if (!pid) {
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
