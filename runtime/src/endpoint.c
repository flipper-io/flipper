#include <flipper/libflipper.h>
#include <flipper/message.h>

/* Creates a new libflipper endpoint. */
struct _lf_endpoint *lf_endpoint_create(const struct _lf_endpoint *constructor, void *record) {
	struct _lf_endpoint *endpoint = calloc(1, sizeof(struct _lf_endpoint));
	lf_assert(endpoint, failure, E_MALLOC, "Failed to allocate memory for new endpoint.");
	memcpy(endpoint, constructor, sizeof(struct _lf_endpoint));
	endpoint -> record = record;
	return endpoint;
failure:
	return NULL;
}

/* Enqueues a message for sending over the endpoint. */
int lf_endpoint_enqueue(struct _lf_endpoint *endpoint, struct _lf_msg *message) {
	return lf_ll_append(&(endpoint -> incoming), message, free);
}

bool lf_endpoint_has_data(struct _lf_endpoint *endpoint) {
	return (endpoint -> incoming) != 0;
}

/* Dequeues the next message to be sent. */
struct _lf_msg *lf_endpoint_dequeue(struct _lf_endpoint *endpoint) {
	return lf_ll_pop(&(endpoint -> incoming));
}

/* Checks to see if any messages are available over the endpoint, and loads them into the incoming queue if there are. This function should never block! */
void lf_endpoint_poll(struct _lf_endpoint *endpoint) {
	/* Checks if there is any data available over this endpoint. If there is, it is loaded into the message processing queue. */

}

int lf_endpoint_release(struct _lf_endpoint *endpoint) {
	lf_assert(endpoint, failure, E_NULL, "NULL pointer provided for endpoint deallocation.");
	endpoint -> destroy(endpoint);
	free(endpoint);
	return lf_success;
failure:
	return lf_error;
}
