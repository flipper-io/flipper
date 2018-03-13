#include <flipper.h>
#include <flipper/atmegau2/megausb.h>

struct _lf_device THIS_DEVICE;

lf_return_t fmr_push(struct _fmr_push_pull_packet *packet) {
	int retval;
	void *swap = malloc(packet->length);
	if (!swap) {
		lf_error_raise(E_MALLOC, NULL);
		return lf_error;
	}
	megausb_bulk_receive(swap, packet->length);
	*(uint64_t *)(packet->call.parameters) = (uintptr_t)swap;
	retval = fmr_execute(packet->call.index, packet->call.function, packet->call.ret, packet->call.argc, packet->call.types, (void *)(packet->call.parameters));
	free(swap);
	return retval;
}

lf_return_t fmr_pull(struct _fmr_push_pull_packet *packet) {
	lf_return_t retval;
	void *swap = malloc(packet->length);
	if (!swap) {
		lf_error_raise(E_MALLOC, NULL);
		return lf_error;
	}
	*(uint64_t *)(packet->call.parameters) = (uintptr_t)swap;
	retval = fmr_execute(packet->call.index, packet->call.function, packet->call.ret, packet->call.argc, packet->call.types, (void *)(packet->call.parameters));
	megausb_bulk_transmit(swap, packet->length);
	free(swap);
	return retval;
}
