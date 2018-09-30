/* Flipper Carbon Board Support Header */

#ifndef __carbon_h__
#define __carbon_h__

#include "atmegau2.h"
#include "atsam4s.h"

struct _carbon_device {
    /* Device that handles interacting with the u2. (ATMEGA16U2) */
    struct _lf_device *_u2;
    /* Microprocessor that handles code execution. (ATSAM4S16B) */
    struct _lf_device *_4s;
};

/* Attaches to all carbon devices. */
struct _carbon_device *carbon_attach(void);
/* Attaches to a carbon device over the network. */
struct _lf_device *carbon_attach_hostname(char *hostname);

struct _lf_device *carbon_u2(struct _carbon_device *carbon);

struct _lf_device *carbon_4s(struct _carbon_device *carbon);

void sam_reset(struct _carbon_device *carbon);
int sam_enter_dfu(struct _carbon_device *carbon);
int sam_exit_dfu(struct _carbon_device *carbon);
int sam_off(struct _carbon_device *carbon);
int sam_on(struct _carbon_device *carbon);

#endif
