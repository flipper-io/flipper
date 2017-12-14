#include <flipper.h>
/* POSIX networking for flipper. */
#include <unistd.h>
#include <arpa/inet.h>
#include <dlfcn.h>
#include <flipper/posix/network.h>

/* fserve - Creates a local server that acts as a virtual flipper device. */

struct _fvm_module {
	char name[32];
	void **functions;
};

struct _fvm_module fvm_modules[16];
int modulec = 0;

struct _lf_endpoint *nep = NULL;

int fld_index(lf_crc_t identifier) {
	lf_debug("Searching for counterpart module to '0x%04x'.", identifier);
	for (int i = 0; i < modulec; i ++) {
		char *name = fvm_modules[i].name;
		lf_crc_t _crc = lf_crc(name, strlen(name) + 1);
		if (_crc == identifier) {
			lf_debug("Found counterpart '%s' at index '%i'.", name, i);
			return i;
		}
	}
	return -1;
}

int fvm_load_module(char *path) {
	void *dlm = dlopen(path, RTLD_LAZY);
	lf_assert(dlm, failure, E_NULL, "Failed to open '%s'.", path);
	struct _lf_module *module = dlsym(dlm, "_module");
	lf_assert(module, failure, E_NULL, "Failed to read package.");
	lf_debug("Loaded package '%s'.", module->name);
	void **jumptable = dlsym(dlm, "_jumptable");
	lf_assert(jumptable, failure, E_NULL, "Failed to read jumptable from package '%s'.", module->name);
	lf_debug("Read jumptable from package '%s'.", module->name);
	struct _fvm_module *m = &fvm_modules[modulec++];
	strcpy(m->name, module->name);
	m->functions = jumptable;
	lf_debug("Successfully loaded package '%s'.", module->name);
	return lf_success;
failure:
	return lf_error;
}

lf_return_t fmr_perform_user_invocation(struct _fmr_invocation *invocation, struct _fmr_result *result) {
	lf_debug("Performing user invocation on function '%i' in module '%i'.", invocation->function, invocation->index);
	lf_assert(invocation->index < modulec, failure, E_BOUNDARY, "Module index was out of bounds.");
	lf_return_t (* function)(void) = fvm_modules[invocation->index].functions[invocation->function];
	Dl_info info;
	int i = dladdr(function, &info);
	lf_assert(i != 0, failure, E_NULL, "Could not resolve dynamic symbol name.");
	lf_debug("Invoking module function '%s'.", info.dli_sname);
	return fmr_call(function, invocation->ret, invocation->argc, invocation->types, invocation->parameters);
failure:
	return lf_error;
}

int main(int argc, char *argv[]) {

	//lf_set_debug_level(LF_DEBUG_LEVEL_ALL);

	if (argc > 1) {
		char **modules = &argv[1];
		for (int i = 0; i < (argc-1); i ++) {
			lf_debug("Loading package '%s'.", *modules);
			fvm_load_module(*modules++);
		}
	}

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

	printf("Flipper Virtual Machine (FVM) v0.1.0\nListening on 'localhost'.\n\n");

	while (1) {
		struct _fmr_packet packet;
		nep->pull(nep, &packet, sizeof(struct _fmr_packet));
		lf_debug_packet(&packet, sizeof(struct _fmr_packet));
		struct _fmr_result result;
		lf_error_clear();
		fmr_perform(&packet, &result);
		lf_debug_result(&result);
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
	*(uint64_t *)(packet->call.parameters) = (uintptr_t)swap;
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
	*(uint64_t *)(packet->call.parameters) = (uintptr_t)swap;
	retval = fmr_execute(packet->call.index, packet->call.function, packet->call.ret, packet->call.argc, packet->call.types, (void *)(packet->call.parameters));
	nep->push(nep, swap, packet->length);
	free(swap);
	return retval;
failure:
	return lf_error;
}
