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
	uint8_t (* const ready)(struct _lf_endpoint *_endpoint);
	/* Sends a single byte through the endpoint. */
	void (* const put)(struct _lf_endpoint *_endpoint, uint8_t byte);
	/* Retrieves a single byte from the endpoint. */
	uint8_t (* const get)(struct _lf_endpoint *_endpoint);
	/* Transmits a block of data through the endpoint. */
	int (* const push)(struct _lf_endpoint *_endpoint, void *source, lf_size_t length);
	/* Receives a block of data from the endpoint. */
	int (* const pull)(struct _lf_endpoint *_endpoint, void *destination, lf_size_t length);
	/* Destroys any state associated with the endpoint. */
	int (* const destroy)();
	/* Tracks endpoint specific configuration information. */
	void *record;
	/* A queue of incoming messages ready to be handled. */
	lf_msg_queue *incoming;
};

struct _lf_endpoint *lf_endpoint_create(const struct _lf_endpoint *constructor, void *record);
int lf_endpoint_enqueue(struct _lf_endpoint *endpoint, struct _lf_msg *message);
bool lf_endpoint_has_data(struct _lf_endpoint *endpoint);
struct _lf_msg *lf_endpoint_dequeue(struct _lf_endpoint *endpoint);
void lf_endpoint_poll(struct _lf_endpoint *endpoint);
int lf_endpoint_release(struct _lf_endpoint *endpoint);

#endif
