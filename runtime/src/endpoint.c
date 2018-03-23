#include <flipper.h>
#include <flipper/message.h>

/* Creates a new libflipper endpoint. */
struct _lf_endpoint *lf_endpoint_create(int (* configure)(struct _lf_endpoint *endpoint, void *_ctx),
										bool (* ready)(struct _lf_endpoint *endpoint),
										int (* push)(struct _lf_endpoint *endpoint, void *source, lf_size_t length),
										int (* pull)(struct _lf_endpoint *endpoint, void *destination, lf_size_t length),
										int (* destroy)(struct _lf_endpoint *endpoint),
										size_t ctx_size) {
	lf_assert(configure && ready && push && pull && destroy, failure, E_NULL, "NULL function provided to '%s'.", __PRETTY_FUNCTION__);
	struct _lf_endpoint *endpoint = calloc(1, sizeof(struct _lf_endpoint));
	lf_assert(endpoint, failure, E_MALLOC, "Failed to allocate memory for new endpoint.");
	endpoint->configure = configure;
	endpoint->ready = ready;
	endpoint->push = push;
	endpoint->pull = pull;
	endpoint->destroy = destroy;
	endpoint->_ctx = calloc(1, ctx_size);
	lf_assert(endpoint->_ctx, failure, E_MALLOC, "Failed to allocate the memory needed to create an endpoint context.");
	return endpoint;
failure:
	return NULL;
}

/* Enqueues a message for sending over the endpoint. */
int lf_endpoint_enqueue(struct _lf_endpoint *endpoint, struct _lf_msg *message) {
	return lf_error;
}

bool lf_endpoint_has_data(struct _lf_endpoint *endpoint) {
	return NULL;
}

/* Dequeues the next message to be sent. */
struct _lf_msg *lf_endpoint_dequeue(struct _lf_endpoint *endpoint) {
	return NULL;
}

/* Checks to see if any messages are available over the endpoint, and loads them into the incoming queue if there are. This function should never block! */
void lf_endpoint_poll(struct _lf_endpoint *endpoint) {
	/* Checks if there is any data available over this endpoint. If there is, it is loaded into the message processing queue. */

}

int lf_endpoint_release(struct _lf_endpoint *endpoint) {
	lf_assert(endpoint, failure, E_NULL, "NULL endpoint provided to '%s'.", __PRETTY_FUNCTION__);
	if (endpoint->destroy) endpoint->destroy(endpoint);
	free(endpoint->_ctx);
	free(endpoint);
	return lf_success;
failure:
	return lf_error;
}
