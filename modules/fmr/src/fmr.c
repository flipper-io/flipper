#define __private_include__
#include <flipper/fmr.h>
#include <flipper/error.h>
#include <flipper/flipper.h>

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
		/* If the argument has overflowed, throw an error. */
		if (value & (1UL << fmr_ovf_shift)) {
			error_raise(E_OVERFLOW, error_message("The argument provided (0x%lx) cannot be sent over FMR without explicit truncation."), value);
		}
		/* Append the argument to the fmr_list. */
		fmr_append(list, (fmr_type)((value >> fmr_type_shift) & 0x7), value);
	}
	/* Release the variadic argument list. */
	va_end(argv);
	return list;
}

void fmr_append(struct _fmr_list *list, fmr_type type, fmr_va value) {
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
	/* Allocate the memory required to stage the argument into the parent list. */
	struct _fmr_arg *argument = (struct _fmr_arg *)calloc(1, sizeof(struct _fmr_arg));
	/* Ensure that the request for memory was satisfied. */
	if (!argument) {
		error_raise(E_MALLOC, error_message("Failed to allocate the memory required to append to the argument list located at %p.", argument, list));
		return;
	}
	/* If the type is implicit, chose a type width for the user. */
	if (type == fmr_implicit_t) {
		if (value < 0x100) {
			type = fmr_int8_t;
		}
		else if (value < 0x10000) {
			type = fmr_int16_t;
		}
		else {
			type = fmr_int32_t;
		}
	}
	/* Write the type and value of the argument into the list. */
	memcpy(argument, &((struct _fmr_arg){ (fmr_arg)value, type, NULL }), sizeof(struct _fmr_arg));
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
	return top;
}

void fmr_free(struct _fmr_list *list) {
	/* Ensure that a valid list was provided. */
	if (!list) {
		error_raise(E_NULL, error_message("An attempt was made to free an invalid argument list."));
		return;
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
}

int fmr_bind(struct _fmr_module *module, char *name) {
	/* Calculate the module's identifier. */
	module -> identifier = lf_checksum(name, strlen(name));
	/* If the module has not already been assigned to a device, assign it to the actively selected device. */
	if (!module -> device) {
		module -> device = flipper.device;
	}
	/* Ask the device if it has a module with the given identifier. */

	return lf_success;
}

int fmr_generate(struct _fmr_module *module, fmr_function function, struct _fmr_list *args, struct _fmr_packet *packet) {
	/* Clear the packet. */
	memset(packet, 0, sizeof(struct _fmr_packet));
	/* Store the argument count in the packet. */
	packet -> target.argc = args -> argc;
	/* Set the magic number. */
	packet -> header.magic = 0xfe;
	/* Calculate the number of bytes needed to encode the widths of the types. */
	uint8_t encode_length = lf_ceiling((args -> argc * 2), 8);
	packet -> header.length = sizeof(struct _fmr_header) + sizeof(struct _fmr_target) + encode_length;
	/* Calculate the offset into the packet at which the arguments will be loaded. */
	uint8_t *offset = (uint8_t *)&(packet -> body) + encode_length;
	/* Create a buffer for encoding argument types. */
	uint32_t types = 0;
	/* Load arguments into the packet, encoding the type of each. */
	for (int i = 0; i < args -> argc; i ++) {
		/* Pop the argument from the argument list. */
		struct _fmr_arg *arg = fmr_pop(args);
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
	packet -> header.checksum = lf_checksum(packet, sizeof(struct _fmr_packet));
 	/* Destroy the argument list. */
	fmr_free(args);
	return lf_success;
}

void fmr_parse(struct _fmr_packet *packet) {
	
}
