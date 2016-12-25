/* network.h - Define and implement the network endpoint. */

#ifndef __lf_network_h__
#define __lf_network_h__
#include <flipper/core.h>

extern struct _lf_endpoint lf_network_ep;

#ifdef __private_include__

/* The default port over which FMR can be accessed. */
#define FMR_PORT 1234

int network_configure(struct _lf_endpoint *endpoint, char *hostname);
uint8_t network_ready(void);
void network_put(uint8_t byte);
uint8_t network_get(void);
int network_push(void *source, lf_size_t length);
int network_pull(void *destination, lf_size_t length);
int network_destroy(struct _lf_endpoint *endpoint);

#endif
#endif
