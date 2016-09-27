#ifndef __fvm_h__
#define __fvm_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* Define the size of virtual NVM. */
#define NVM_SIZE 2 * 1024 * (528 * 2)

/* Declare all global variables. */
extern uint8_t *v_nvm;

extern struct _lf_endpoint lf_fvm_ep;

/* Declare function prototypes. */
void fvm_begin(void);
void fvm_end(void);

int fvm_configure(struct _lf_endpoint *endpoint);
uint8_t fvm_ready(void);
void fvm_put(uint8_t byte);
uint8_t fvm_get(void);
int fvm_push(void *source, lf_size_t length);
int fvm_pull(void *destination, lf_size_t length);
int fvm_destroy(struct _lf_endpoint *endpoint);

/* Packet debugging function provided by libflipper. */
extern void lf_debug_packet(struct _fmr_packet *packet);

#endif
