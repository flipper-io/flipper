/* Flipper: Carbon Edition board header file. */
#include <flipper/libflipper.h>

/* NOTE: Probably move this again. */
extern struct _lf_endpoint lf_bridge_ep;
/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
extern int lf_bridge_configure();
extern uint8_t lf_bridge_ready(struct _lf_endpoint *this);
extern void lf_bridge_put(struct _lf_endpoint *this, uint8_t byte);
extern uint8_t lf_bridge_get(struct _lf_endpoint *this);
extern int lf_bridge_push(struct _lf_endpoint *this, void *source, lf_size_t length);
extern int lf_bridge_pull(struct _lf_endpoint *this, void *destination, lf_size_t length);
extern int lf_bridge_destroy(struct _lf_endpoint *this);
