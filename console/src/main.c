#include <stdio.h>
#define __private_include__
#include <flipper/flipper.h>
#include <flipper/fmr.h>
#include <platform/posix.h>

int main(int argc, char *argv[]) {

	flipper_attach();

	char raw[FMR_PACKET_SIZE];
	raw[0] = atoi(argv[1]);
	raw[1] = atoi(argv[2]);
	raw[2] = atoi(argv[3]);
	libusb_push(raw, FMR_PACKET_SIZE);
	libusb_pull(raw, FMR_PACKET_SIZE);

	flipper_exit();

    return 0;
}
