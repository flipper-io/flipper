#include "libflipper.h"

struct _lf_arg *lf_arg_create(lf_type type, lf_arg value) {

	struct _lf_arg *arg = malloc(sizeof(struct _lf_arg));
	lf_assert(arg, E_MALLOC, "failed to allocate new lf_arg");

	arg->type = type;
	arg->value = value;

	return arg;
fail:
	return NULL;
}

void lf_arg_release(struct _lf_arg *arg) {

	lf_assert(arg, E_NULL, "invalid argument");

	free(arg);

fail:
	return;
}

struct _lf_ll *fmr_build(int argc, ...) {

	va_list argv;
    struct _lf_ll *list = NULL;
    struct _lf_arg *arg = NULL;

	lf_assert(argc < FMR_MAX_ARGC, E_OVERFLOW, "Too many arguments were provided when building (%i) call.", argc);

	/* Initialize the va_list that we created above. */
	va_start(argv, argc);

	/* Walk the variadic argument list, appending arguments to the list created above. */
	while (argc --) {

		int type = va_arg(argv, int);
		lf_arg value = va_arg(argv, lf_arg);

		lf_assert(type <= lf_max_t, E_TYPE, "An invalid type was provided while appending the parameter '%llx' with type '%x' to the argument list.", value, type);

		arg = lf_arg_create(type, value);
		lf_assert(arg, E_MALLOC, "failed to append new lf_arg");

		lf_ll_append(&list, arg, lf_arg_release);
	}

	va_end(argv);

	return list;
fail:

	lf_ll_release(&list);
	va_end(argv);

	return NULL;
}

int lf_create_call(lf_module module, lf_function function, lf_type ret, struct _lf_ll *args, struct _fmr_header *header, struct _fmr_call *call) {

    size_t argc = 0;
    uint8_t *offset = NULL;

    lf_assert(header, E_NULL, "invalid header");
	lf_assert(call, E_NULL, "invalid call");

	/* Store the target module, function, and argument count in the packet. */
	argc = lf_ll_count(args);

	call->module = module;
	call->function = function;
	call->ret = ret;
	call->argc = argc;

	/* Calculate the offset into the packet at which the arguments will be loaded. */
	offset = (uint8_t *)&(call->argv);

	/* Load arguments into the packet, encoding the type of each. */
	for (size_t i = 0; i < argc; i ++) {

        struct _lf_arg *arg = NULL;
        uint8_t size = 0;

		/* Pop the argument from the argument list. */
		arg = lf_ll_item(args, i);
		lf_assert(arg, E_NULL, "invalid argument supplied");

		/* Encode the argument's type. */
		call->argt |= (arg->type & lf_max_t) << (i * 4);

		/* Calculate the size of the argument. */
		size = lf_sizeof(arg->type);

		/* Copy the argument into the parameter segment. */
		memcpy(offset, &(arg->value), size);

		/* Increment the offset appropriately. */
		offset += size;

		/* Increment the size of the packet. */
		header->len += size;
	}

	lf_ll_release(&args);

	return lf_success;
fail:

	lf_ll_release(&args);

	return lf_error;
}

int fmr_rpc(struct _lf_device *device, struct _fmr_call_packet *packet, lf_return_t *retval) {

    struct _lf_module *m;
    lf_return_t (* f)(void) = NULL;

    struct _fmr_call call = packet->call;

    m = lf_ll_item(device->modules, call.module);
    lf_assert(f, E_NULL, "bad module lookup");

    f = m->interface[call.function];
	lf_assert(f, E_NULL, "bad interface address");

	*retval = fmr_call(f, call.ret, call.argc, call.argt, call.argv);

	return lf_success;
fail:
	return lf_error;
}

int fmr_push(struct _lf_device *device, struct _fmr_push_pull_packet *packet) {

    void *dst = (void *)(uintptr_t)packet->ptr;
    size_t len = (size_t)packet->len;

	lf_assert(device->read(device, dst, len) , E_FMR, "failed to pull data");

	return lf_success;
fail:
	return lf_error;
}

int fmr_pull(struct _lf_device *device, struct _fmr_push_pull_packet *packet) {

    void *src = (void *)(uintptr_t)packet->ptr;
    size_t len = (size_t)packet->len;

    lf_assert(device->write(device, src, len) , E_FMR, "failed to push data");

	return lf_success;
fail:
	return lf_error;
}

int fmr_dyld(struct _lf_device *device, struct _fmr_dyld_packet *packet, lf_return_t *retval) {

    struct _lf_module *module = dyld_module(device, packet->module);
	lf_assert(module, E_MODULE, "no module '%s' has been registered", module->name);

	*retval = module->idx;

	return lf_success;
fail:
	return lf_error;
}

int fmr_malloc(struct _fmr_memory_packet *packet, lf_return_t *retval) {

    void *ptr = NULL;
    size_t size = (size_t)packet->size;

    ptr = malloc(size);
	lf_assert(ptr, E_MALLOC, "failed to allocate memory");

	*retval = (lf_return_t)(uintptr_t)ptr;

	return lf_success;
fail:
	return lf_error;
}

int fmr_free(struct _fmr_memory_packet *packet) {

    void *ptr = (void *)(uintptr_t)packet->ptr;

	free(ptr);

	return lf_success;
}

int fmr_perform(struct _lf_device *device, struct _fmr_packet *packet) {

    struct _fmr_header *hdr = &packet->hdr;
    struct _fmr_result result;
	lf_return_t retval = -1;
    lf_crc_t _crc, crc;
	int e = E_UNIMPLEMENTED;

	/* Check that the magic number matches. */
	lf_assert(hdr->magic == FMR_MAGIC_NUMBER, E_CHECKSUM, "invalid magic number");

	/* Ensure the packet's checksums match. */
	_crc = hdr->crc;
    hdr->crc = 0;
	lf_crc(packet, hdr->len, &crc);
	lf_assert(!memcmp(&_crc, &crc, sizeof(crc)), E_CHECKSUM, "checksums do not match (0x%04x/0x%04x)", _crc, crc);

    /* clear error state */
	lf_error_set(E_OK);

	/* Switch through the packet subclasses and invoke the appropriate handler for each. */
	switch (hdr->type) {
        /* rpc */
		case fmr_rpc_class:
			e = fmr_rpc(device, (struct _fmr_call_packet *)packet, &retval);
		break;
        /* push */
		case fmr_push_class:
			e = fmr_push(device, (struct _fmr_push_pull_packet *)packet);
		break;
        /* pull */
		case fmr_pull_class:
			e = fmr_pull(device, (struct _fmr_push_pull_packet *)packet);
		break;
        /* dyld */
		case fmr_dyld_class:
			e = fmr_dyld(device, (struct _fmr_dyld_packet *)packet, &retval);
		break;
        /* malloc */
		case fmr_malloc_class:
			e = fmr_malloc((struct _fmr_memory_packet *)packet, &retval);
		break;
        /* free */
		case fmr_free_class:
			e = fmr_free((struct _fmr_memory_packet *)packet);
		break;
        /* default */
		default:
			e = E_UNIMPLEMENTED;
		break;
	}

fail:

	result.error = lf_error_get();
	result.value = retval;

	e = device->write(device, &result, sizeof(struct _fmr_result));

	lf_debug_result(&result);

	return e;
}
