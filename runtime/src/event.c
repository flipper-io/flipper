#include <flipper/libflipper.h>

struct _lf_event *lf_event_create(lf_event_id _id, lf_event_handler handler, void *_ctx) {
    struct _lf_event *event = malloc(sizeof(struct _lf_event));
    lf_assert(event, failure, E_NULL, "NULL");
    memset(event, 0, sizeof(struct _lf_event));
    event -> id = _id;
    event -> handler = handler;
    event -> ctx = _ctx;
    return event;
failure:
	return NULL;
}

/* Returns an unused event id. */
lf_event_id lf_event_generate_unique_id(void) {
    return 0;
}

/* Tears down an event and its observers, removing it from the event system. */
int lf_event_release(lf_event *event) {
    lf_assert(event, failure, E_NULL, "NULL");
    /* Tear down all of the observers registered to this event. */
    lf_ll_release(&(event -> observers));
    free(event);
    return lf_success;
failure:
	return lf_error;
}

/* Registers an event with the event system and assigns its handler. This is done on ALL platforms that interact with a given event identifier. */
lf_event *lf_event_register(lf_event_id id, lf_event_handler handler, void *ctx) {
    struct _lf_event *event = lf_event_create(id, handler, ctx);
    lf_assert(event, failure, E_NULL, "NULL");
    lf_ll_append(&lf_get_event_list(), event, lf_event_release);
    return event;
failure:
	return NULL;
}

void *lf_event_finder(void *_event, void *_id) {
    struct _lf_event *event = _event;
    lf_event_id id = (lf_event_id)_id;
    if (event -> id == id) {
        return event;
    }
    return NULL;
}

struct _lf_event *lf_event_for_id(lf_event_id id) {
    void *id_as_void_ptr = (void *)(uintptr_t)id;
    return lf_ll_apply_func(lf_get_event_list(), id_as_void_ptr, lf_event_finder);
}

/* Causes a local event to be triggered when events of the same identifier are triggered on the device. */
int lf_event_subscribe(lf_event *event, struct _lf_device *device) {
    lf_assert(event && device, failure, E_NULL, "NULL");
    /* Register the link on the device. */
    event -> device = device;
    /* CALL TO DEVICE HERE */
    lf_observer_register(device -> endpoint, event -> id);
    return lf_success;
failure:
	return lf_error;
}

/* Triggers an event causing its observers to be notified. */
int lf_event_trigger(lf_event *event) {
    /* Trigger all of the event's observers. */
    lf_ll_apply_func(event -> observers, NULL, lf_observer_notify);
    /* If there is a callback, call it. */
    if (event -> handler) {
        event -> handler(event);
    }
    return lf_success;
}

/* This function is called for each of the attached devices. */
void *lf_event_handler_func(void *_device, void *_other) {
    struct _lf_device *device = _device;
    lf_assert(device, failure, E_NULL, "NULL");
    /* Handle all of the messages available over the device's endpoint. */
    while (lf_endpoint_has_data(device -> endpoint)) {
        /* Dequeue a message from the endpoint's incoming message queue. */
        struct _lf_msg *msg = lf_endpoint_dequeue(device -> endpoint);
        lf_assert(msg, failure, E_NULL, "NULL");
        /* Apply the message to the world. */
        lf_msg_apply(msg);
    }
failure:
    return NULL;
}

void lf_handle_events(void) {
    for (;;) {
        /* Handle events across all attached devices. */
        if (lf_get_device_list()) {
            lf_ll_apply_func(lf_get_device_list(), NULL, lf_event_handler_func);
        }
        // usleep(10000);
        /* Avoid tail-call optimization. */
        __asm__ __volatile__ ("");
    }
}
