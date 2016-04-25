#ifndef __network_h__
#define __network_h__

#include <flipper/core.h>

#define FLIPPER_NETWORK_PORT 7777

extern const struct _bus network;

#ifdef __private_include__

enum { _network_configure, _network_enable, _network_disable, _network_ready, _network_put, _network_get, _network_push, _network_pull };

void network_configure(void *ip);
void network_enable(void);
void network_disable(void);
uint8_t network_ready(void);
void network_put(uint8_t byte);
uint8_t network_get(void);
void network_push(void *source, size_t length);
void network_pull(void *source, size_t length);

#endif
#endif
