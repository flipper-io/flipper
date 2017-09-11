#ifndef __lf_endpoint_h__
#define __lf_endpoint_h__

#include <flipper/types.h>
#include <flipper/ll.h>
#include <flipper/message.h>

typedef struct _lf_ll lf_msg_queue;

/* Standardizes interaction with a physical hardware bus for the transmission of arbitrary data. */
struct _lf_endpoint {
	/* Indicates whether or not the endpoint is ready to send or receive data. */
	bool (* ready)(struct _lf_endpoint *endpoint);
	/* Transmits a block of data through the endpoint. */
	int (* push)(struct _lf_endpoint *endpoint, void *source, lf_size_t length);
	/* Receives a block of data from the endpoint. */
	int (* pull)(struct _lf_endpoint *endpoint, void *destination, lf_size_t length);
	/* Destroys any state associated with the endpoint. */
	int (* destroy)(struct _lf_endpoint *endpoint);
	/* Tracks endpoint specific context. */
	void *_ctx;
};

struct _lf_endpoint *lf_endpoint_create(bool (* ready)(struct _lf_endpoint *endpoint),
										int (* push)(struct _lf_endpoint *_endpoint, void *source, lf_size_t length),
										int (* pull)(struct _lf_endpoint *_endpoint, void *destination, lf_size_t length),
										int (* destroy)(struct _lf_endpoint *endpoint),
										size_t record_size);
int lf_endpoint_enqueue(struct _lf_endpoint *endpoint, struct _lf_msg *message);
bool lf_endpoint_has_data(struct _lf_endpoint *endpoint);
struct _lf_msg *lf_endpoint_dequeue(struct _lf_endpoint *endpoint);
void lf_endpoint_poll(struct _lf_endpoint *endpoint);
int lf_endpoint_release(struct _lf_endpoint *endpoint);

#endif
