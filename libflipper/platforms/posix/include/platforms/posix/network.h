/* network.h - Define and implement the network endpoint. */

#ifndef __lf_network_h__
#define __lf_network_h__
#include <flipper/core.h>

extern struct _lf_endpoint lf_network_ep;

#ifdef __private_include__

/* The default port over which FMR can be accessed. */
#define FMR_PORT 1234

int network_configure(struct _lf_endpoint *this, char *hostname);
uint8_t network_ready(struct _lf_endpoint *this);
void network_put(struct _lf_endpoint *this, uint8_t byte);
uint8_t network_get(struct _lf_endpoint *this);
int network_push(struct _lf_endpoint *this, void *source, lf_size_t length);
int network_pull(struct _lf_endpoint *this, void *destination, lf_size_t length);
int network_destroy(struct _lf_endpoint *this);

#endif
#endif
