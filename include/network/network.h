#ifndef network_h

#define network_h

#include <flipper/types.h>

#include <bus/bus.h>

#define FLIPPER_NETWORK_PORT 7777

extern const struct _bus network;

#ifdef __private_include__

enum { _network_configure, _network_enable, _network_disable, _network_ready, _network_put, _network_get, _network_push, _network_pull };

extern void network_configure(char *ip);

extern void network_enable(void);

extern void network_disable(void);

extern bool network_ready(void);

extern void network_put_byte(uint8_t byte);

extern uint8_t network_get_byte(void);

extern void network_push(void *source, uint32_t length);

extern void network_pull(void *source, uint32_t length);

#endif

#endif