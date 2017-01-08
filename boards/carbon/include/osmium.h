#ifndef __osmium_h__
#define __osmium_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/libflipper.h>

/* Include all of the module headers. */
#include <flipper/carbon.h>

#ifdef __private_include__

#define OS_UART_BAUDRATE 115200

/* Initializes platform specific periphery and prepares system for kernel configuration. */
extern void system_init(void);
/* Performs a variety of system related tasks during idle. */
extern void system_task(void);
/* Deinitializes platform specific periphery and prepares system for shutdown. */
extern void system_deinit(void);
/* Resets the system. */
extern void system_reset(void);

/* Initializes the scheduler. */
void os_task_init(void);
typedef uint32_t os_stack_t;
/* Creates a task. */
int task_create(void *handler, os_stack_t *stack, uint32_t stack_size);

#endif
#endif
