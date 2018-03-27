/* network.h - Define and implement the network endpoint. */

#ifndef __lf_network_h__
#define __lf_network_h__

#include <flipper.h>

#include <arpa/inet.h>
#include <netdb.h>

/* The default port over which FMR can be accessed. */
#define LF_UDP_PORT 3258

struct _lf_network_context {
	int fd;
	char host[64];
	struct sockaddr_in device;
};

int lf_network_configure(struct _lf_device *device, void *_ctx);
bool lf_network_ready(struct _lf_device *device);
int lf_network_push(struct _lf_device *device, void *source, lf_size_t length);
int lf_network_pull(struct _lf_device *device, void *destination, lf_size_t length);
int lf_network_destroy(struct _lf_endpoint *endpoint);
struct _lf_endpoint *lf_network_endpoint_for_hostname(char *hostname);

/* Returns the endpoint for a device on the network. */
struct _lf_endpoint *lf_network_endpoint_for_hostname(char *hostname);

#endif
