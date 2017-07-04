#ifndef __lf_observer_h__
#define __lf_observer_h__

#include <flipper/types.h>
#include <flipper/event.h>
#include <flipper/endpoint.h>

struct _lf_observer {
    /* The event id which this observer is subscribed to. */
    lf_event_id event_id;
    /* The endpoint over which the observer is to be notified. */
    struct _lf_endpoint *endpoint;
};

typedef struct _lf_ll *lf_observer_list;

struct _lf_observer *lf_observer_create(lf_event_id _id, struct _lf_endpoint *_endpoint);
int lf_observer_register(struct _lf_endpoint *endpoint, lf_event_id id);
void *lf_observer_notify(void *_observer, void *_unused);

#endif
