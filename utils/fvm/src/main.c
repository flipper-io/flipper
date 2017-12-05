#include <flipper.h>
/* POSIX networking for flipper. */
#include <unistd.h>
#include <arpa/inet.h>
#include <flipper/posix/network.h>

/* fserve - Creates a local server that acts as a virtual flipper device. */

struct _lf_endpoint *nep = NULL;

int main(int argc, char *argv[]) {

	/* Create a UDP server. */
	struct sockaddr_in addr;
	int sd = socket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
	if (sd < 0) {
		printf("Failed to get socket.\n");
		return 0;
	}
	bzero(&addr, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(LF_UDP_PORT);
	addr.sin_addr.s_addr = htonl(INADDR_ANY);
	int _e = bind(sd, (struct sockaddr*)&addr, sizeof(addr));
	if (_e < 0) {
		printf("Failed to create server.\n");
		return 0;
	}

	/* The network endpoint for the virtual flipper device. */
	struct _lf_network_context *context = NULL;
	nep = lf_endpoint_create(lf_network_configure,
												  lf_network_ready,
												  lf_network_push,
												  lf_network_pull,
												  lf_network_destroy,
												  sizeof(struct _lf_network_context));
	lf_assert(nep, failure, E_ENDPOINT, "Failed to create endpoint for networked device.");
	context = (struct _lf_network_context *)nep->_ctx;
	context->fd = sd;

	lf_set_debug_level(LF_DEBUG_LEVEL_ALL);

	while (1) {
		struct _fmr_packet packet;
		nep->pull(nep, &packet, sizeof(struct _fmr_packet));
		lf_debug_packet(&packet, sizeof(struct _fmr_packet));
		struct _fmr_result result;
		lf_error_clear();
		fmr_perform(&packet, &result);
		nep->push(nep, &result, sizeof(struct _fmr_result));
	}

	close(sd);

failure:
	return EXIT_FAILURE;
}

lf_return_t fmr_push(struct _fmr_push_pull_packet *packet) {
	int retval;
	void *swap = malloc(packet->length);
	lf_assert(swap, failure, E_MALLOC, "Failed to allocate push buffer");
	nep->pull(nep, swap, packet->length);
	*(uint32_t *)(packet->call.parameters) = (uintptr_t)swap;
	retval = fmr_execute(packet->call.index, packet->call.function, packet->call.ret, packet->call.argc, packet->call.types, (void *)(packet->call.parameters));
	free(swap);
	return retval;
failure:
	return lf_error;
}

lf_return_t fmr_pull(struct _fmr_push_pull_packet *packet) {
	lf_return_t retval;
	void *swap = malloc(packet->length);
	lf_assert(swap, failure, E_MALLOC, "Failed to allocate pull buffer");
	*(uint32_t *)(packet->call.parameters) = (uintptr_t)swap;
	retval = fmr_execute(packet->call.index, packet->call.function, packet->call.ret, packet->call.argc, packet->call.types, (void *)(packet->call.parameters));
	nep->push(nep, swap, packet->length);
	free(swap);
	return retval;
failure:
	return lf_error;
}
