#define __private_include__
#include <private/nvm.h>
#include <platform/fvm.h>
#include <flipper/fmr.h>

/* Pointer to the start of virtual non-volatile memory. */
uint8_t *v_nvm;

const struct _lf_endpoint lf_fvm_ep = {
	fvm_configure,
	fvm_ready,
	fvm_put,
	fvm_get,
	fvm_push,
	fvm_pull,
	fvm_destroy
};

void fvm_begin(void) {
	/* Allocate the memory needed to emulate NVM. */
	v_nvm = (uint8_t *)malloc(NVM_SIZE);
	/* Format NVM. */
	nvm_format();
}

void fvm_end(void) {
	/* Free the memory used to emulate virtual NVM. */
	free(v_nvm);
}

int fvm_configure(struct _lf_endpoint *endpoint) {
	return 0;
}

uint8_t fvm_ready(void) {
	return 0;
}

void fvm_put(uint8_t byte) {

}

uint8_t fvm_get(void) {
	return 0;
}

int fvm_push(void *source, lf_size_t length) {
	struct _fmr_packet packet;
	memcpy(&packet, source, length);

	printf("- Message runtime packet deconstruction. -\n\n");
	printf("header:\n");
	printf("\t└─ magic:\t0x%x\n", packet.header.magic);
	printf("\t└─ checksum:\t0x%x\n", packet.header.checksum);
	printf("\t└─ length:\t%d bytes\n", packet.header.length);
	printf("target:\n");
	printf("\t└─ module:\t0x%x\n", packet.target.module);
	printf("\t└─ function:\t0x%x\n", packet.target.function);
	printf("\t└─ argc:\t0x%x (%d arguments)\n", packet.target.argc, packet.target.argc);
	printf("arguments:\n");
	/* Calculate the number of bytes needed to encode the widths of the types. */
	uint8_t encode_length = lf_ceiling((packet.target.argc * 2), 8);
	/* Calculate the offset into the packet at which the arguments will be loaded. */
	uint8_t *offset = packet.body + encode_length;
	/* Create a buffer for encoding argument types. */
	uint32_t types = 0;
	memcpy(&types, packet.body, encode_length);
	char *typestrs[] = { "fmr_int8", "fmr_int16", "fmr_int32" };
	for (int i = 0; i < packet.target.argc; i ++) {
		fmr_type type = types & 0x3;
		fmr_arg arg = 0;
		memcpy(&arg, offset, fmr_sizeof(type));
		printf("\t└─ %s:\t0x%x\n", typestrs[type], arg);
		offset += fmr_sizeof(type);
		types >>= 2;
	}
	printf("\nRaw packet data:\n\n");
	for (int i = 1; i <= FMR_PACKET_SIZE; i ++) {
		printf("0x%02x ", *(uint8_t *)(source + i - 1));
		if (i != 0 && i % 8 == 0) printf("\n");
	}
	printf("\n");

	return lf_success;
}

int fvm_pull(void *destination, lf_size_t length) {
	return 0;
}

int fvm_destroy(struct _lf_endpoint *endpoint) {
	if (v_nvm) {
		printf("FVM was destroyed successfully.\n");
		free(v_nvm);
		v_nvm = NULL;
	}
	return lf_success;
}
