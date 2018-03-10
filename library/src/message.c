#include <flipper.h>
#include <flipper/message.h>
#include <flipper/event.h>

/* Creates a stub message body for a given kind of message. */
struct _lf_msg *lf_msg_create(lf_msg_kind kind) {
    struct _lf_msg *msg = malloc(sizeof(struct _lf_msg));
    lf_assert(msg, failure, E_NULL, "NULL");
    memset(msg, 0, sizeof(struct _lf_msg));
    msg -> kind = kind;
    msg -> event_id = 0;
    return msg;
failure:
	return NULL;
}

/* Sends a message to a specified device. This function blocks until the response message has been received. */
int lf_msg_send(struct _lf_msg *msg, struct _lf_endpoint *endpoint) {
    return lf_success;
}

/* Creates a message receipt event for the outgoing packet. */
int lf_msg_subscribe_receipt(struct _lf_msg *msg, lf_event_handler_func callback) {
    /* Generate a unique id to receive the message receipt event over. */
    lf_event_id id = lf_event_generate_unique_id();
    lf_event_register(id, callback, NULL);
    /* Set the message's recepit event id. */
    msg -> event_id = id;
    return lf_success;
}

/* Asynchronously sends a message to the given device. Invokes the callback function with the response message when the message returns. */
int lf_msg_send_async(struct _lf_msg *msg, struct _lf_endpoint *endpoint, lf_event_handler_func callback) {
    if (callback) {
        /* Subscribe the handler provided to the message's response. */
        lf_msg_subscribe_receipt(msg, callback);
    }
    /* Queue the message to be sent over the device's endpoint. */
    lf_endpoint_enqueue(endpoint, msg);
    return lf_success;
}

/* Handles a message locally. */
int lf_msg_apply(struct _lf_msg *msg) {
    lf_assert(msg, failure, E_NULL, "NULL");
    /* Switch on the message type. */
    switch (msg -> kind) {
        case lf_msg_event_kind:
            /* If the message has a callback event, trigger it. */
            if (msg -> event_id) {
                struct _lf_event *receipt_event = lf_event_for_id(msg -> event_id);
                if (!receipt_event) {
                    break;
                }
                lf_event_trigger(receipt_event);
            }
            break;
        case lf_msg_rpc_kind:
            /* Send to the RPC handler here. */

            break;
        default:
            return lf_error;
            break;
    }
    return lf_success;
failure:
	return lf_error;
}
