#define __private_include__
#include <flipper/carbon.h>
#include <flipper/atmegau2/megausb.h>

fmr_return fmr_push(struct _fmr_push_pull_packet *packet) {
	fmr_return retval = 0xdeadbeef;
	void *swap = malloc(packet->length);
	if (!swap) {
		lf_error_raise(E_MALLOC, NULL);
		return -1;
	}
	megausb_bulk_receive(swap, packet->length);
	*(uintptr_t *)(packet->call.parameters) = (uintptr_t)swap;
	retval = fmr_execute(packet->call.index, packet->call.function, packet->call.argc, packet->call.types, (void *)(packet->call.parameters));
	free(swap);
	return retval;
}

fmr_return fmr_pull(struct _fmr_push_pull_packet *packet) {
	fmr_return retval = 0xdeadbeef;
	void *swap = malloc(packet->length);
	if (!swap) {
		lf_error_raise(E_MALLOC, NULL);
		return -1;
	}
	*(uintptr_t *)(packet->call.parameters) = (uintptr_t)swap;
	retval = fmr_execute(packet->call.index, packet->call.function, packet->call.argc, packet->call.types, (void *)(packet->call.parameters));
	megausb_bulk_transmit(swap, packet->length);
	free(swap);
	return retval;
}
