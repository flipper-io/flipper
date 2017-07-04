#ifndef __lf_endpoint_h__
#define __lf_endpoint_h__

#include <flipper/types.h>
#include <flipper/ll.h>
#include <flipper/message.h>

typedef struct _lf_ll lf_msg_queue;

/* Standardizes interaction with a physical hardware bus for the transmission of arbitrary data. */
struct _lf_endpoint {
	/* Configures the endpoint record given an arbitrary set of parameters. */
	int (* configure)();
	/* Indicates whether or not the endpoint is ready to send or receive data. */
	uint8_t (* ready)(struct _lf_endpoint *this);
	/* Sends a single byte through the endpoint. */
	void (* put)(struct _lf_endpoint *this, uint8_t byte);
	/* Retrieves a single byte from the endpoint. */
	uint8_t (* get)(struct _lf_endpoint *this);
	/* Transmits a block of data through the endpoint. */
	int (* push)(struct _lf_endpoint *this, void *source, lf_size_t length);
	/* Receives a block of data from the endpoint. */
	int (* pull)(struct _lf_endpoint *this, void *destination, lf_size_t length);
	/* Destroys any state associated with the endpoint. */
	int (* destroy)();
	/* Tracks endpoint specific configuration information. */
	void *record;
	/* A queue of incoming messages ready to be handled. */
	lf_msg_queue *incoming;
};

int lf_endpoint_enqueue(struct _lf_endpoint *endpoint, struct _lf_msg *message);
bool lf_endpoint_has_data(struct _lf_endpoint *endpoint);
struct _lf_msg *lf_endpoint_dequeue(struct _lf_endpoint *endpoint);
void lf_endpoint_poll(struct _lf_endpoint *endpoint);
int lf_endpoint_release(struct _lf_endpoint *endpoint);

#endif
