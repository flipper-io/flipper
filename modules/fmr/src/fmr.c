#define __private_include__
#include <flipper/fmr.h>
#include <flipper/error.h>
#include <flipper/modules.h>

/* Define the virtual interface for this module. */
const struct _fmr fmr = {
	fmr_push,
	fmr_pull
};

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

int fmr_generate(fmr_module module, fmr_function function, struct _fmr_list *parameters, struct _fmr_packet *packet) {
	/* Ensure that the pointer to the outgoing packet is valid. */
	if (!packet) {
		error_raise(E_NULL, error_message("Invalid packet reference provided during message runtime packet generation."));
		return lf_error;
	} else if (!parameters) {
		/* If no arguments are provided, automatically provide an empty argument list. */
		parameters = fmr_build(0);
	}
	/* Zero the packet. */
	memset(packet, 0, sizeof(struct _fmr_packet));
	/* Set the magic number. */
	packet -> header.magic = 0xfe;
	packet -> header.checksum = 0x00;
	/* If the module's identifier is in the range of identifiers reserved for the standard modules, make this packet invoke a standard module. */
	if (module < _std_module_id_max) {
		packet -> target.attributes |= LF_STANDARD_MODULE;
	}
	/* Store the target module, function, and argument count in the packet. */
	packet -> target.module = module;
	packet -> target.function = function;
	packet -> target.argc = parameters -> argc;
	/* Calculate the number of bytes needed to encode the widths of the types. */
	uint8_t encode_length = lf_ceiling((parameters -> argc * 2), 8);
	/* Compute the initial length of the packet. */
	packet -> header.length = sizeof(struct _fmr_header) + sizeof(struct _fmr_target) + encode_length;
	/* Calculate the offset into the packet at which the arguments will be loaded. */
	uint8_t *offset = (uint8_t *)&(packet -> body) + encode_length;
	/* Create a buffer for encoding argument types. */
	uint32_t types = 0;
	/* Load arguments into the packet, encoding the type of each. */
	int argc = parameters -> argc;
	for (int i = 0; i < argc; i ++) {
		/* Pop the argument from the argument list. */
		struct _fmr_arg *arg = fmr_pop(parameters);
		/* Encode the argument's type. */
		types |= (arg -> type & 0x3) << (i * 2);
		/* Calculate the size of the argument. */
		uint8_t size = fmr_sizeof(arg -> type);
		/* Copy the argument into the packet body. */
		memcpy(offset, &(arg -> value), size);
		/* Increment the offset appropriately. */
		offset += size;
		/* Increment the size of the packet. */
		packet -> header.length += size;
		/* Release the argument. */
		free(arg);
	}
	/* Copy the encoded type widths into the packet. */
	memcpy(&(packet -> body), &types, encode_length);
	/* Calculate the packet checksum. */
	packet -> header.checksum = lf_checksum(packet, packet -> header.length);
	 /* Destroy the argument list. */
	fmr_free(parameters);
	return lf_success;
}

int fmr_perform(struct _fmr_packet *packet, struct _fmr_result *result) {
	/* Temporarily store the packet's checksum. */
	lf_id_t _cs = packet -> header.checksum;
	/* Zero the checksum of the packet. */
	packet -> header.checksum = 0x00;
	/* Calculate our checksum of the packet. */
	uint16_t cs = lf_checksum(packet, packet -> header.length);
	/* Ensure that the checksums of the packets match. */
	if (_cs != cs) {
		error_raise(E_CHECKSUM, NULL);
		goto done;
	}
	/* Calculate the number of bytes needed to dencode the widths of the types. */
	uint8_t dencode_length = lf_ceiling((packet -> target.argc * 2), 8);
	/* Create a buffer for decoding argument types. */
	fmr_types types = 0;
	/* Copy the encoded type widths into the buffer. */
	memcpy(&types, (void *)(&(packet -> body)), dencode_length);
	/* If the module is a standard moudle, obtain it internally. */
	if (packet -> target.attributes & LF_STANDARD_MODULE) {
		/* Execute the function. */
		result -> value = fmr_execute(packet -> target.module, packet -> target.function, packet -> target.argc, types, (void *)(packet -> body + dencode_length));
	} else {
		error_raise(E_MODULE, NULL);
	}
done:
	/* Catalogue any error state generated by the procedure. */
	result -> error = error_get();
	return lf_success;
}
