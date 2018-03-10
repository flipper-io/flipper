#include <flipper.h>

struct _lf_observer *lf_observer_create(lf_event_id _id, struct _lf_endpoint *_endpoint) {
    struct _lf_observer *observer = malloc(sizeof(struct _lf_observer));
    lf_assert(observer, failure, E_NULL, "NULL");
    observer -> event_id = _id;
    observer -> endpoint = _endpoint;
    return observer;
failure:
	return NULL;
}

/* Registers the endpoint over which the last message was recieved as an observer to an event. */
#warning This is a device function.
int lf_observer_register(struct _lf_endpoint *endpoint, lf_event_id id) {
    lf_assert(endpoint, failure, E_NULL, "NULL");
    struct _lf_observer *observer = lf_observer_create(id, endpoint);
    lf_assert(observer, failure, E_NULL, "NULL");
    /* Obtain the event that we are registering this observer to. */
    struct _lf_event *event = lf_event_for_id(id);
    lf_assert(event, failure, E_NULL, "NULL");
    return lf_ll_append(&(event -> observers), observer, free);
failure:
	return lf_error;
}

void lf_observer_notify(const void *_observer, void *_unused) {
    struct _lf_observer *observer = (struct _lf_observer *)_observer;
    lf_assert(observer, failure, E_NULL, "NULL");
    /* Send a message to the observer to notify it that an event was triggered. */
    struct _lf_msg *msg = lf_msg_create(lf_msg_event_kind);
    msg -> event_id = observer -> event_id;
    lf_msg_send(msg, observer -> endpoint);
failure:
    return;
}
