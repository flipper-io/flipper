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

lf_return_t fmr_execute(lf_module module, lf_function function, lf_type ret, lf_argc argc, lf_types argt, void *arguments) {
	/* Dereference the pointer to the target module. */
	struct _lf_module *m = lf_ll_item(lf_get_current_device()->modules, module);
	/* Dereference and return a pointer to the target function. */
	void *address = m->interface[function];
	/* Ensure that the function address is valid. */
	lf_assert(address, failure, E_NULL, "NULL address supplied to '%s'.", __PRETTY_FUNCTION__);
	/* Perform the function call internally. */
	return fmr_call(address, ret, argc, argt, arguments);
failure:
	return lf_error;
}

lf_return_t fmr_dyld(struct _fmr_dyld_packet *packet) {
	lf_debug("Trying to find module '%s'.", packet->module);
	struct _lf_module *module = dyld_module(lf_get_current_device(), packet->module);
	lf_assert(module, failure, E_MODULE, "No module '%s' has been registered.", packet->module);
	return module->idx;
failure:
	return lf_error;
}

/* ~ Message runtime subclass handlers. ~ */

int fmr_perform(struct _fmr_packet *packet, struct _fmr_result *result) {
	/* Check that the magic number matches. */
	lf_assert(packet->header.magic == FMR_MAGIC_NUMBER, failure, E_CHECKSUM, "Invalid magic number.");

	/* Ensure the packet's checksums match. */
	lf_crc_t _crc = packet->header.checksum;
	packet->header.checksum = 0x00;
	uint16_t crc = lf_crc(packet, packet->header.length);
	lf_assert(_crc == crc, failure, E_CHECKSUM, "Checksums do not match.");

	/* Cast the incoming packet to the different packet structures for subclass handling. */
	struct _fmr_invocation *call = &((struct _fmr_invocation_packet *)packet)->call;

	/* Switch through the packet subclasses and invoke the appropriate handler for each. */
	switch (packet->header.type) {
		case fmr_execute_class:
			result->value = fmr_execute(call->index, call->function, call->ret, call->argc, call->types, call->parameters);
		break;
		case fmr_push_class:
			result->value = fmr_push((struct _fmr_push_pull_packet *)(packet));
		break;
		case fmr_pull_class:
			result->value = fmr_pull((struct _fmr_push_pull_packet *)(packet));
		break;
		case fmr_dyld_class:
			result->value = fmr_dyld((struct _fmr_dyld_packet *)(packet));
		break;
		default:
			lf_assert(true, failure, E_SUBCLASS, "An invalid message runtime subclass was provided.");
		break;
	}

	result->error = lf_error_get();
	return lf_success;
failure:
	result->error = lf_error_get();
	return lf_error;
}
