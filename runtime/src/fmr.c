#define __private_include__
#include <flipper/libflipper.h>

/* TODO: Move this implementation to use lf_ll. */

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
		fmr_va value = va_arg(argv, fmr_va);
		fmr_type type = (fmr_type)((value >> (sizeof(fmr_arg) * 8)) & 0x7);
		lf_assert(type < fmr_int32_t, failure, E_TYPE, "An invalid type was provided while appending the parameter '0x%08x' to the argument list.", (fmr_arg)value);
		struct _lf_arg *arg = malloc(sizeof(struct _lf_arg));
		arg->value = (fmr_arg)value;
		arg->type = type;
		lf_assert(arg, failure, E_MALLOC, "Failed to allocate new lf_arg.");
		lf_ll_append(&list, arg, free);
	}
	/* Release the variadic argument list. */
	va_end(argv);
	return list;
failure:
	lf_ll_release(&list);
	return NULL;
}

int fmr_create_call(fmr_module module, fmr_function function, struct _lf_ll *args, struct _fmr_header *header, struct _fmr_invocation *call) {
	lf_assert(header || call, failure, E_NULL, "No header or call.");
	/* Store the target module, function, and argument count in the packet. */
	size_t argc = lf_ll_count(args);
	call->index = module;
	call->function = function;
	call->argc = argc;
	/* Calculate the offset into the packet at which the arguments will be loaded. */
	uint8_t *offset = (uint8_t *)&(call->parameters);
	/* Load arguments into the packet, encoding the type of each. */
	for (int i = 0; i < argc; i ++) {
		/* Pop the argument from the argument list. */
		struct _lf_arg *arg = lf_ll_pop(&args);
		/* Encode the argument's type. */
		call->types |= (arg->type & 0x3) << (i * 2);
		/* Calculate the size of the argument. */
		uint8_t size = fmr_sizeof(arg->type);
		/* Copy the argument into the parameter segment. */
		memcpy(offset, &(arg->value), size);
		/* Increment the offset appropriately. */
		offset += size;
		/* Increment the size of the packet. */
		header->length += size;
		/* Release the argument. */
		free(arg);
	}
	 /* Destroy the argument list. */
	lf_ll_release(&args);
	return lf_success;
failure:
	lf_ll_release(&args);
	return lf_error;
}

fmr_return fmr_execute(fmr_module module, fmr_function function, fmr_argc argc, fmr_types types, void *arguments) {
	/* Dereference the pointer to the target module. */
	const void *object = (const void *)(fmr_modules[module]);
	/* Dereference and return a pointer to the target function. */
	const void *address = ((const void **)(object))[function];
	/* Ensure that the function address is valid. */
	if (!address) {
		lf_error_raise(E_RESOULTION, NULL);
		return 0;
	}
	/* Perform the function call internally. */
	return fmr_call(address, argc, types, arguments);
}

/* ~ Message runtime subclass handlers. ~ */

LF_WEAK int fmr_perform_standard_invocation(struct _fmr_invocation *call, struct _fmr_result *result) {
	result->value = fmr_execute(call->index, call->function, call->argc, call->types, call->parameters);
	return result->error = lf_error_get();
}

LF_WEAK int fmr_perform_user_invocation(struct _fmr_invocation *invocation, struct _fmr_result *result) {
	return lf_error;
}

LF_WEAK int fmr_configuration_subclass_handler(struct _fmr_result *result) {
	
	return result->error = lf_error_get();
}

int fmr_perform(struct _fmr_packet *packet, struct _fmr_result *result) {
	/* Check that the magic number matches. */
	if (packet->header.magic != FMR_MAGIC_NUMBER) {
		lf_error_raise(E_CHECKSUM, NULL);
		goto failure;
	}
	/* Create a copy of the packet's checksum. */
	lf_crc_t _crc = packet->header.checksum;
	/* Clear the checksum of the packet. */
	packet->header.checksum = 0x00;
	/* Calculate our checksum of the packet. */
	uint16_t crc = lf_crc(packet, packet->header.length);
	/* Ensure that the checksums of the packets match. */
	if (_crc != crc) {
		lf_error_raise(E_CHECKSUM, NULL);
		goto failure;
	}

	/* Cast the incoming packet to the different packet structures for subclass handling. */
	struct _fmr_invocation_packet *_ip = (struct _fmr_invocation_packet *)packet;

	/* Switch through the packet subclasses and invoke the appropriate handler for each. */
	switch (packet->header.class) {
		case fmr_configuration_class:
			fmr_configuration_subclass_handler(result);
		break;
		case fmr_standard_invocation_class:
			fmr_perform_standard_invocation(&(_ip->call), result);
		break;
		case fmr_user_invocation_class:
			fmr_perform_user_invocation(&(_ip->call), result);
		break;
		case fmr_ram_load_class:
		case fmr_send_class:
		case fmr_push_class:
			result->value = fmr_push((struct _fmr_push_pull_packet *)(packet));
		break;
		case fmr_receive_class:
		case fmr_pull_class:
			result->value = fmr_pull((struct _fmr_push_pull_packet *)(packet));
		break;
		case fmr_event_class:
		break;
		default:
			lf_assert(true, failure, E_SUBCLASS, "An invalid message runtime subclass was provided.");
		break;
	};

	return lf_success;
failure:
	return lf_error;
}
