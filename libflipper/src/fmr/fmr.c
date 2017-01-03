#define __private_include__
#include <flipper/libflipper.h>

/* If defined, the symbols needed to create FMR packets will be defined. */
#ifdef __fmr_generators__

struct _fmr_list *fmr_build(fmr_argc argc, ...) {
	/* Ensure that the argument count is within bounds. */
	if (argc > FMR_MAX_ARGC) {
		error_raise(E_OVERFLOW, error_message("The maximum number of arguments (%i) was reached while trying to build the argument list.", FMR_MAX_ARGC));
		return NULL;
	}
	/* Allocate memory for a new fmr_list. */
	struct _fmr_list *list = (struct _fmr_list *)calloc(1, sizeof(struct _fmr_list));
	if (!list) {
		error_raise(E_MALLOC, error_message("Failed to allocate the memory required to create a new argument list."));
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
		/* Allocate the memory required to stage the argument into the parent list. */
		struct _fmr_arg *argument = (struct _fmr_arg *)calloc(1, sizeof(struct _fmr_arg));
		/* Ensure that the request for memory was satisfied. */
		if (!argument) {
			error_raise(E_MALLOC, error_message("Failed to allocate the memory required to append to the argument list located at %p.", list));
			free(list);
			/* Nullify the argument list pointer for the failed return. */
			list = NULL;
			break;
		}
		fmr_type type = (fmr_type)((value >> (sizeof(fmr_arg) * 8)) & 0x7);
		if (type > fmr_int32_t) {
			error_raise(E_TYPE, error_message("An invalid type was provided while appending the parameter '0x%08x' to the argument list.", (fmr_arg)value));
		}
		/* Write the type and value of the argument into the list. */
		memcpy(argument, &((struct _fmr_arg){ (fmr_arg)value, type, NULL }), sizeof(struct _fmr_arg));
		/* Append the argument to the fmr_list. */
		fmr_append(list, argument);
	}
	/* Release the variadic argument list. */
	va_end(argv);
	return list;
}

void fmr_append(struct _fmr_list *list, struct _fmr_arg *argument) {
	/* Ensure that a valid list was provided. */
	if (!list) {
		error_raise(E_NULL, error_message("An attempt was made to append to an invalid argument list."));
		return;
	}
	/* Ensure that the argument count is within bounds. */
	if (list -> argc >= FMR_MAX_ARGC) {
		error_raise(E_OVERFLOW, error_message("The maximum number of arguments (%i) was reached while appending to the argument list located at %p.", FMR_MAX_ARGC, list));
		return;
	}
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
	}
	else {
		list -> argv = argument;
	}
	/* Advance the argument count of the list. */
	list -> argc ++;
}

struct _fmr_list *fmr_merge(struct _fmr_list *first, struct _fmr_list *second) {
	if (!second) {
		goto done;
	}
	/* Pop each argument from the second argument list and append it to the first. */
	while (second -> argc) {
		fmr_append(first, fmr_pop(second));
	}
done:
	return first;
}

struct _fmr_arg *fmr_pop(struct _fmr_list *list) {
	/* Ensure that a valid list was provided. */
	if (!list) {
		error_raise(E_NULL, error_message("An attempt was made to pop from an invalid argument list."));
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

int fmr_free(struct _fmr_list *list) {
	/* Ensure that a valid list was provided. */
	if (!list) {
		error_raise(E_NULL, error_message("An attempt was made to free an invalid argument list."));
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

int fmr_generate(fmr_module module, fmr_function function, struct _fmr_list *parameters, struct _fmr_header *header, struct _fmr_call *call) {
	/* Ensure that the pointer to the outgoing packet is valid. */
	if (!header || !call) {
		error_raise(E_NULL, error_message("Invalid header or call reference provided during message runtime packet generation."));
		return lf_error;
	} else if (!parameters) {
		/* If no arguments are provided, automatically provide an empty argument list. */
		parameters = fmr_build(0);
	}
	/* Set the magic number. */
	header -> magic = FMR_MAGIC_NUMBER;
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

#endif

fmr_return fmr_execute(fmr_module module, fmr_function function, fmr_argc argc, fmr_types types, void *arguments) {
	/* Dereference the pointer to the target module. */
	const void *object = (const void *)(fmr_modules[module]);
	/* Dereference and return a pointer to the target function. */
	const void *address = ((const void **)(object))[function];
	/* Ensure that the function address is valid. */
	if (!address) {
		error_raise(E_RESOULTION, NULL);
		return 0;
	}
	/* Perform the function call internally. */
	return fmr_call(address, argc, types, arguments);
}

fmr_return fmr_perform_invocation(struct _fmr_invocation_packet *packet) {
	/* Perform the function invocation. */
	return fmr_execute(packet -> call.index, packet -> call.function, packet -> call.argc, packet -> call.types, (void *)(packet -> call.parameters));
}

int fmr_perform(struct _fmr_packet *packet, struct _fmr_result *result) {
	/* Check that the magic number matches. */
	if (packet -> header.magic != FMR_MAGIC_NUMBER) {
		error_raise(E_CHECKSUM, NULL);
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
		error_raise(E_CHECKSUM, NULL);
		goto failure;
	}
	/* Swap space for push/pull transfers. */
	void *swap = NULL;
	/* Packet cast for the push pull packet. */
	struct _fmr_push_pull_packet *ppacket = (struct _fmr_push_pull_packet *)(packet);
	/* Switch through the classes of packets. */
	switch (packet -> header.class) {
		case fmr_configuration_class:
			/* Send the configuration information back. */
			lf_self.endpoint -> push(lf_self.endpoint, &lf_self.configuration, sizeof(struct _lf_configuration));
		break;
		/* NOTE: Right now standard invocations and user invocations are done the same way. This should change. */
		case fmr_standard_invocation_class:
			/* Perform an invocation on a standard module. */
			result -> value = fmr_perform_invocation((struct _fmr_invocation_packet *)(packet));
		break;
		case fmr_user_invocation_class:

		break;
		case fmr_push_class:
			swap = malloc(ppacket -> length);
			if (!swap) {
				error_raise(E_MALLOC, NULL);
				break;
			}
			lf_self.endpoint -> pull(lf_self.endpoint, swap, ppacket -> length);
			/* Insert the swap address. NOTE: This is not cross platform. REMOVE THIS */
			*(uint16_t *)(ppacket -> call.parameters) = (uintptr_t)swap;
			fmr_execute(ppacket -> call.index, ppacket -> call.function, ppacket -> call.argc, ppacket -> call.types, (void *)(ppacket -> call.parameters));
			free(swap);
		break;
		case fmr_pull_class:
			swap = malloc(ppacket -> length);
			if (!swap) {
				error_raise(E_MALLOC, NULL);
				break;
			}
			/* Insert the swap address. NOTE: This is not cross platform. REMOVE THIS */
			*(uint16_t *)(ppacket -> call.parameters) = (uintptr_t)swap;
			fmr_execute(ppacket -> call.index, ppacket -> call.function, ppacket -> call.argc, ppacket -> call.types, (void *)(ppacket -> call.parameters));
			lf_self.endpoint -> push(lf_self.endpoint, swap, ppacket -> length);
			free(swap);
		break;
		case fmr_event_class:
			/* Handle an event. */
			// Call into event subsystem
		break;
		default:
			/* Bad class value. */
		break;
	};
done:
	/* Catalogue any error state generated by the procedure. */
	result -> error = error_get();
	return lf_success;
failure:
	/* NOTE: Remove this edge case. System freezes when an incorrect number of packets are sent. In this case, due to failure. */
	if (packet -> header.class == fmr_configuration_class) {
		/* Send the configuration information back. */
		lf_self.endpoint -> push(lf_self.endpoint, &lf_self.configuration, sizeof(struct _lf_configuration));
	}
	/* Catalogue any error state generated by the procedure. */
	result -> error = error_get();
	return lf_error;
}
