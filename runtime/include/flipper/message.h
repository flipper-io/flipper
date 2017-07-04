#ifndef __lf_message_h__
#define __lf_message_h__

#include <flipper/types.h>
#include <flipper/event.h>

typedef enum {
    lf_msg_rpc_kind,
    lf_msg_event_kind,
} lf_msg_kind;

/* A message with intent to modify device state. */
struct _lf_msg {
    /* The raw data conveyed by the message. */
    void *_raw;
    /* The size of the raw data conveyed by the message. */
    /* The kind of message. */
    lf_msg_kind kind;
    /* The outgoing message's number. */
    int message_number;
    /* The receipt event id. */
    lf_event_id event_id;
};

struct _lf_msg *lf_msg_create(lf_msg_kind kind);
int lf_msg_send(struct _lf_msg *msg, struct _lf_endpoint *endpoint);
int lf_msg_subscribe_receipt(struct _lf_msg *msg, lf_event_handler callback);
int lf_msg_send_async(struct _lf_msg *msg, struct _lf_endpoint *endpoint, lf_event_handler callback);
int lf_msg_apply(struct _lf_msg *msg);

#endif
