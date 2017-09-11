/* network.h - Define and implement the network endpoint. */

#ifndef __lf_network_h__
#define __lf_network_h__
#include <flipper/types.h>

/* The default port over which FMR can be accessed. */
#define FMR_PORT 1234

/* Returns the endpoint for a device on the network. */
struct _lf_endpoint *lf_network_endpoint_for_hostname(char *hostname);

#endif
