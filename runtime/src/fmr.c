#include <flipper.h>

struct _lf_arg *lf_arg_create(lf_type type, lf_arg value) {
	struct _lf_arg *arg = malloc(sizeof(struct _lf_arg));
	lf_assert(arg, failure, E_MALLOC, "Failed to allocate new lf_arg.");
	arg->type = type;
	arg->value = value;
	return arg;
failure:
	return NULL;
}

void lf_arg_release(struct _lf_arg *arg) {
	if (arg) free(arg);
}

struct _lf_ll *fmr_build(int argc, ...) {
	lf_assert(argc < FMR_MAX_ARGC, failure, E_OVERFLOW, "Too many arguments were provided when building (%i) call.", argc);
	struct _lf_ll *list = NULL;
	/* Construct a va_list to access variadic arguments. */
	va_list argv;
	/* Initialize the va_list that we created above. */
	va_start(argv, argc);
	/* Walk the variadic argument list, appending arguments to the list created above. */
	while (argc --) {
		/* Unstage the value of the argument from the variadic argument list. */
		int type = va_arg(argv, int);
		lf_arg value = va_arg(argv, lf_arg);
		lf_assert(type <= lf_max_t, failure, E_TYPE, "An invalid type was provided while appending the parameter '%llx' with type '%x' to the argument list.", value, type);
		struct _lf_arg *arg = lf_arg_create(type, value);
		lf_assert(arg, failure, E_MALLOC, "Failed to append new lf_arg.");
		lf_ll_append(&list, arg, lf_arg_release);
	}
	va_end(argv);
	return list;
failure:
	lf_ll_release(&list);
	va_end(argv);
	return NULL;
}

int lf_create_call(lf_module module, lf_function function, lf_type ret, struct _lf_ll *args, struct _fmr_header *header, struct _fmr_invocation *call) {
	lf_assert(header, failure, E_NULL, "NULL header passed to '%s'.", __PRETTY_FUNCTION__);
	lf_assert(call, failure, E_NULL, "NULL call passed to '%s'.", __PRETTY_FUNCTION__);
	/* Store the target module, function, and argument count in the packet. */
	size_t argc = lf_ll_count(args);
	call->index = module;
	call->function = function;
	call->ret = ret;
	call->argc = argc;
	/* Calculate the offset into the packet at which the arguments will be loaded. */
	uint8_t *offset = (uint8_t *)&(call->parameters);
	/* Load arguments into the packet, encoding the type of each. */
	for (size_t i = 0; i < argc; i ++) {
		/* Pop the argument from the argument list. */
		struct _lf_arg *arg = lf_ll_item(args, i);
		lf_assert(arg, failure, E_NULL, "Invalid argument supplied to '%s'.", __PRETTY_FUNCTION__);
		/* Encode the argument's type. */
		call->types |= (arg->type & lf_max_t) << (i * 4);
		/* Calculate the size of the argument. */
		uint8_t size = lf_sizeof(arg->type);
		/* Copy the argument into the parameter segment. */
		memcpy(offset, &(arg->value), size);
		/* Increment the offset appropriately. */
		offset += size;
		/* Increment the size of the packet. */
		header->length += size;
	}
	lf_ll_release(&args);
	return lf_success;
failure:
	lf_ll_release(&args);
	return lf_error;
}

int fmr_execute(struct _lf_device *device, lf_module module, lf_function function, lf_type ret, lf_argc argc, lf_types argt, void *arguments, lf_return_t *retval) {
	struct _lf_module *m = lf_ll_item(device->modules, module);
	void *f = m->interface[function];
	lf_assert(f, failure, E_NULL, "Bad function address in '%s'.", __FUNCTION__);
	*retval = fmr_call(f, ret, argc, argt, arguments);
	return lf_success;

failure:
	return lf_error;
}

int fmr_push(struct _lf_device *device, lf_module module, lf_function function, lf_size_t length, lf_return_t *retval) {
	void *source = malloc(length);
	lf_assert(source, failure, E_MALLOC, "Failed to allocate memory for '%s'.", __FUNCTION__);
	int e = device->read(device, source, length);
	lf_assert(e == lf_success, failure, E_FMR, "Failed to pull data for '%s'.", __FUNCTION__);

	struct _lf_module *m = lf_ll_item(device->modules, module);
	int (* push)(void *source, lf_size_t length) = m->interface[function];
	lf_assert(push, failure, E_NULL, "Bad function address in '%s'.", __FUNCTION__);
	*retval = push(source, length);

	free(source);
	return lf_success;

failure:
	return lf_error;
}

int fmr_pull(struct _lf_device *device, lf_module module, lf_function function, lf_size_t length, lf_return_t *retval) {
	void *destination = malloc(length);
	lf_assert(destination, failure, E_MALLOC, "Failed to allocate memory for '%s'.", __FUNCTION__);

	struct _lf_module *m = lf_ll_item(device->modules, module);
	int (* pull)(void *destination, lf_size_t length) = m->interface[function];
	lf_assert(pull, failure, E_NULL, "Bad function address in '%s'.", __FUNCTION__);
	*retval = pull(destination, length);

	int e = device->write(device, destination, length);
	lf_assert(e == lf_success, failure, E_FMR, "Failed to push data for '%s'.", __FUNCTION__);

	free(destination);
	return lf_success;

failure:
	return lf_error;
}

int fmr_dyld(struct _lf_device *device, char *module, lf_return_t *retval) {
	lf_debug("Trying to find module '%s'.", module);
	struct _lf_module *m = dyld_module(device, module);
	lf_assert(m, failure, E_MODULE, "No module '%s' has been registered.", m->name);
	*retval = m->idx;
	return lf_success;

failure:
	return lf_error;
}

int fmr_perform(struct _lf_device *device, struct _fmr_packet *packet) {
	int e = E_UNIMPLEMENTED;
	lf_return_t retval = -1;

	/* Check that the magic number matches. */
	lf_assert(packet->header.magic == FMR_MAGIC_NUMBER, failure, E_CHECKSUM, "Invalid magic number.");

	/* Ensure the packet's checksums match. */
	lf_crc_t _crc = packet->header.checksum;
	packet->header.checksum = 0x00;
	uint16_t crc = lf_crc(packet, packet->header.length);
	lf_assert(_crc == crc, failure, E_CHECKSUM, "Checksums do not match.");

	/* Cast the incoming packet to the different packet structures for subclass handling. */
	struct _fmr_invocation_packet *ipacket = (struct _fmr_invocation_packet *)packet;
	struct _fmr_push_pull_packet *ppacket = (struct _fmr_push_pull_packet *)packet;
	struct _fmr_dyld_packet *dpacket = (struct _fmr_dyld_packet *)packet;
	struct _fmr_invocation *icall = &ipacket->call;
	struct _fmr_invocation *pcall = &ppacket->call;

	/* Switch through the packet subclasses and invoke the appropriate handler for each. */
	switch (packet->header.type) {
		case fmr_execute_class:
			e = fmr_execute(device, icall->index, icall->function, icall->ret, icall->argc, icall->types, icall->parameters, &retval);
		break;
		case fmr_push_class:
			e = fmr_push(device, pcall->index, pcall->function, ppacket->length, &retval);
		break;
		case fmr_pull_class:
			e = fmr_pull(device, pcall->index, pcall->function, ppacket->length, &retval);
		break;
		case fmr_dyld_class:
			e = fmr_dyld(device, dpacket->module, &retval);
		break;
		default:
			e = E_UNIMPLEMENTED;
		break;
	}

	struct _fmr_result result;
failure:
	result.error = e;
	result.value = retval;
	e = device->write(device, &result, sizeof(struct _fmr_result));
	lf_debug_result(&result);
	return e;
}
