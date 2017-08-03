#ifndef __lf_event_h__
#define __lf_event_h__

/* Include all types exposed by libflipper. */
#include <flipper/types.h>

typedef uint32_t lf_event_id;

#include <flipper/observer.h>

typedef struct _lf_event {
	/* The event's identifier. */
	lf_event_id id;
	/* The event's handler function, if any. */
	void (* handler)(struct _lf_event *event);
	/* The event info that is to be passed to the event handler. */
	struct _lf_device *device;
	/* The event context pointer. */
	void *ctx;
	/* A linked list of observers subscribed to this event. */
	lf_observer_list observers;
} lf_event;

typedef void (* lf_event_handler_func)(lf_event *event);

struct _lf_event *lf_event_create(lf_event_id _id, lf_event_handler_func handler, void *_ctx);
lf_event_id lf_event_generate_unique_id(void);
int lf_event_release(lf_event *event);
lf_event *lf_event_register(lf_event_id id, lf_event_handler_func handler, void *ctx);
void *lf_event_finder(void *_event, void *_id);
struct _lf_event *lf_event_for_id(lf_event_id id);
int lf_event_subscribe(lf_event *event, struct _lf_device *device);
int lf_event_trigger(lf_event *event);
void lf_handle_events(void);

#endif
