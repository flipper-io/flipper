#define __private_include__
#include <flipper/libflipper.h>

struct _fmr_parameters *fmr_build(fmr_argc argc, ...) {
	/* Ensure that the argument count is within bounds. */
	if (argc > FMR_MAX_ARGC) {
		lf_error_raise(E_OVERFLOW, error_message("The maximum number of arguments (%i) was reached while trying to build the argument list.", FMR_MAX_ARGC));
		return NULL;
	}
	/* Allocate memory for a new fmr_parameters. */
	struct _fmr_parameters *list = (struct _fmr_parameters *)calloc(1, sizeof(struct _fmr_parameters));
	if (!list) {
		lf_error_raise(E_MALLOC, error_message("Failed to allocate the memory required to create a new argument list."));
		return NULL;
	}
	/* Construct a va_list to access variadic arguments. */
	va_list argv;
	/* Initialize the va_list that we created above. */
	va_start(argv, argc);
	/* Walk the variadic argument list, appending arguments to the list created above. */
	while (argc --) {
		/* Unstage the value of the argument from the variadic argument list. */
		fmr_va value = va_arg(argv, fmr_va);
		fmr_type type = (fmr_type)((value >> (sizeof(fmr_arg) * 8)) & 0x7);
		if (type > fmr_int32_t) {
			lf_error_raise(E_TYPE, error_message("An invalid type was provided while appending the parameter '0x%08x' to the argument list.", (fmr_arg)value));
		}
		/* Append the argument to the fmr_parameters. */
		int _e = fmr_append(list, type, value);
		if (_e < lf_success) {
			lf_error_raise(E_FMR, error_message("Failed to append to argument list."));
			/* Free the memory allocated to build the list. */
			free(list);
			return NULL;
		}
	}
	/* Release the variadic argument list. */
	va_end(argv);
	return list;
}

int fmr_append(struct _fmr_parameters *list, fmr_type type, fmr_arg value) {
	/* Ensure that a valid list was provided. */
	if (!list) {
		lf_error_raise(E_NULL, error_message("An attempt was made to append to an invalid argument list."));
		return lf_error;
	}
	/* Ensure that the argument count is within bounds. */
	if (list -> argc >= FMR_MAX_ARGC) {
		lf_error_raise(E_OVERFLOW, error_message("The maximum number of arguments (%i) was reached while appending to the argument list located at %p.", FMR_MAX_ARGC, list));
		return lf_error;
	}
	/* Allocate the memory required to stage the argument into the parent list. */
	struct _fmr_arg *argument = (struct _fmr_arg *)calloc(1, sizeof(struct _fmr_arg));
	/* Ensure that the request for memory was satisfied. */
	if (!argument) {
		lf_error_raise(E_MALLOC, error_message("Failed to allocate the memory required to append to the argument list located at %p.", list));
		return lf_error;
	}
	/* Write the type and value into the argument. */
	memcpy(argument, &((struct _fmr_arg){ type, value, NULL }), sizeof(struct _fmr_arg));
	/* Obtain the first argument in the parent list. */
	struct _fmr_arg *tail = list -> argv;
	/* If we already have a first argument, walk to the end of the parent list. */
	if (tail) {
		while (tail -> next) {
			tail = tail -> next;
		}
	}
	/* Save the reference to the new argument into the parent list. */
	if (tail) {
		tail -> next = argument;
	} else {
		list -> argv = argument;
	}
	/* Advance the argument count of the list. */
	list -> argc ++;
	return lf_success;
}

struct _fmr_parameters *fmr_merge(struct _fmr_parameters *first, struct _fmr_parameters *second) {
	if (second) {
		/* Pop each argument from the second argument list and append it to the first. */
		while (second -> argc) {
			struct _fmr_arg *arg = fmr_pop(second);
			int _e = fmr_append(first, arg -> type, arg -> value);
			free(arg);
			if (_e < lf_success) {
				return NULL;
			}
		}
	}
	return first;
}

struct _fmr_arg *fmr_pop(struct _fmr_parameters *list) {
	/* Ensure that a valid list was provided. */
	if (!list) {
		lf_error_raise(E_NULL, error_message("An attempt was made to pop from an invalid argument list."));
		return NULL;
	}
	/* Save the top level argument. */
	struct _fmr_arg *top = list -> argv;
	/* Remove the top argument from the linked list if it exists. */
	if (top) {
		list -> argv = top -> next;
	}
	/* Destroy the link to the next argument. */
	top -> next = NULL;
	/* Decrement the argument count of the list. */
	list -> argc --;
	return top;
}

int fmr_free(struct _fmr_parameters *list) {
	/* Ensure that a valid list was provided. */
	if (!list) {
		lf_error_raise(E_NULL, error_message("An attempt was made to free an invalid argument list."));
		return lf_error;
	}
	/* Obtain the first argument in the parent list. */
	struct _fmr_arg *tail = list -> argv;
	/* If we have more than one argument, walk the parent list. */
	if (tail) {
		/* As we walk the list, free each argument as we go. */
		while (tail -> next) {
			/* Obtain a pointer to the next argument. */
			struct _fmr_arg *next = tail -> next;
			/* Destroy the reference to the current argument. */
			free(tail);
			/* Advance to the next argument. */
			tail = next;
		}
	}
	/* Destroy the list. */
	free(list);
	return lf_success;
}

int fmr_create_call(fmr_module module, fmr_function function, struct _fmr_parameters *parameters, struct _fmr_header *header, struct _fmr_invocation *call) {
	/* Ensure that the pointer to the outgoing packet is valid. */
	if (!header || !call) {
		lf_error_raise(E_NULL, error_message("Invalid header or call reference provided during message runtime packet generation."));
		return lf_error;
	} else if (!parameters) {
		/* If no arguments are provided, automatically provide an empty argument list. */
		parameters = fmr_build(0);
	}
	/* Store the target module, function, and argument count in the packet. */
	call -> index = module;
	call -> function = function;
	call -> argc = parameters -> argc;
	/* Calculate the offset into the packet at which the arguments will be loaded. */
	uint8_t *offset = (uint8_t *)&(call -> parameters);
	/* Load arguments into the packet, encoding the type of each. */
	int argc = parameters -> argc;
	for (int i = 0; i < argc; i ++) {
		/* Pop the argument from the argument list. */
		struct _fmr_arg *arg = fmr_pop(parameters);
		/* Encode the argument's type. */
		call -> types |= (arg -> type & 0x3) << (i * 2);
		/* Calculate the size of the argument. */
		uint8_t size = fmr_sizeof(arg -> type);
		/* Copy the argument into the parameter segment. */
		memcpy(offset, &(arg -> value), size);
		/* Increment the offset appropriately. */
		offset += size;
		/* Increment the size of the packet. */
		header -> length += size;
		/* Release the argument. */
		free(arg);
	}
	 /* Destroy the argument list. */
	fmr_free(parameters);
	return lf_success;
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
	result -> value = fmr_execute(call -> index, call -> function, call -> argc, call -> types, call -> parameters);
	return result -> error = lf_error_get();
}

LF_WEAK int fmr_perform_user_invocation(struct _fmr_invocation *invocation, struct _fmr_result *result) {
	return lf_error;
}

LF_WEAK int fmr_configuration_subclass_handler(struct _fmr_result *result) {
	result -> value = lf_self.endpoint -> push(lf_self.endpoint, &lf_self.configuration, sizeof(struct _lf_configuration));
	return result -> error = lf_error_get();
}

int fmr_perform(struct _fmr_packet *packet, struct _fmr_result *result) {
	/* Check that the magic number matches. */
	if (packet -> header.magic != FMR_MAGIC_NUMBER) {
		lf_error_raise(E_CHECKSUM, NULL);
		goto failure;
	}
	/* Create a copy of the packet's checksum. */
	lf_crc_t _crc = packet -> header.checksum;
	/* Clear the checksum of the packet. */
	packet -> header.checksum = 0x00;
	/* Calculate our checksum of the packet. */
	uint16_t crc = lf_crc(packet, packet -> header.length);
	/* Ensure that the checksums of the packets match. */
	if (_crc != crc) {
		lf_error_raise(E_CHECKSUM, NULL);
		goto failure;
	}

	/* Cast the incoming packet to the different packet structures for subclass handling. */
	struct _fmr_invocation_packet *_ip = (struct _fmr_invocation_packet *)packet;

	/* Switch through the packet subclasses and invoke the appropriate handler for each. */
	switch (packet -> header.class) {
		case fmr_configuration_class:
			fmr_configuration_subclass_handler(result);
		break;
		case fmr_standard_invocation_class:
			fmr_perform_standard_invocation(&(_ip -> call), result);
		break;
		case fmr_user_invocation_class:
			fmr_perform_user_invocation(&(_ip -> call), result);
		break;
		case fmr_ram_load_class:
		case fmr_send_class:
		case fmr_push_class:
			result -> value = fmr_push((struct _fmr_push_pull_packet *)(packet));
		break;
		case fmr_receive_class:
		case fmr_pull_class:
			result -> value = fmr_pull((struct _fmr_push_pull_packet *)(packet));
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
