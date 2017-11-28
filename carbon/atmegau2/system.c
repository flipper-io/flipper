#define __private_include__
#include <flipper/carbon.h>
#include <flipper/atmegau2/megausb.h>

void system_task(void) {
	while (1) {
		/* Pet the watchdog. */
		//wdt_reset();
		struct _fmr_packet packet;
		struct _fmr_result result;
		int8_t _e = megausb_bulk_receive((uint8_t *)(&packet), sizeof(struct _fmr_packet));
		if (_e == lf_success) {
			lf_error_clear();
			fmr_perform(&packet, &result);
			megausb_bulk_transmit((uint8_t *)(&result), sizeof(struct _fmr_result));
		}
	}
}

void os_task_init() {
	system_task();
}

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
