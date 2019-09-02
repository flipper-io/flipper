/* Flipper Carbon Board Support Header */

#ifndef __carbon_h__
#define __carbon_h__

#include <atmegau2.h>
#include <atsam4s.h>

struct _carbon_context {
    /* Device that handles interacting with the u2. (ATMEGA16U2) */
    struct _lf_device *_u2;
    /* Microprocessor that handles code execution. (ATSAM4S16B) */
    struct _lf_device *_4s;
};

/* Attaches to all carbon devices. */
struct _lf_device *carbon_attach(void);
/* Attaches to a carbon device over the network. */
struct _lf_device *carbon_attach_hostname(char *hostname);

int carbon_select_u2(struct _lf_device *device);

void sam_reset(void);
int sam_enter_dfu(void);
int sam_exit_dfu(void);
int sam_off(void);
int sam_on(void);

#endif
