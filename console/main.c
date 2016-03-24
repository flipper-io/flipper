#define __private_include__
#include <flipper/flipper.h>
#include "console.h"
#include <fs/crc.h>
#include <unistd.h>

int main(int argc, char *argv[]) {

	if (argc < 2) {
		printf("\nUsage: flipper [load | io | flash]\n\n");
		return EXIT_SUCCESS;
	}

	/* ~ Attatch this instance of libflipper to the first device present over USB. ~ */
	flipper.attach(FLIPPER_SOURCE_USB);

	error.disclose();

	if (!strcmp(argv[1], "flash")) {
		sam_load_firmware(argv[2]);
	}

	if (!strcmp(argv[1], "load") || !strcmp(argv[1], "debug")) {

		/* ~ Parse user input from the variadic argument list. ~ */
		char *bid = argv[2], *path = argv[3];

		sam.power(0);
		usleep(1000);

		fs_format();

		/* ~ Send the file to the device. ~ */
		fs_transfer_file(path, bid);

		sam.power(1);

		/* ~ Let the operating system boot. ~ */
		usleep(100000);

		uint16_t key = checksum(bid, strlen(bid));

		/* ~ Call the the Flipper Dynamic Loader to begin the dynamic loading process. ~ */
		if (!strcmp(argv[1], "load")) fdl.load(key); else fdl.launch(key);

	}

	else if (!strcmp(argv[1], "launch")) {

		char *bid = argv[2];
		uint16_t key = checksum(bid, strlen(bid));

		/* ~ Launch the program. ~ */
		fdl.launch(key);

	}

	else if (!strcmp(argv[1], "unload")) {

		sam.power(0);
		uint16_t zero = 0;

		/* ~ Zero the FDL break value. ~ */
		fdl_write_config(zero, fdl_config_brk);

		sam.power(1);

	}

	else if (!strcmp(argv[1], "reset")) {

		sam.suspend();
		sam.engage();

	}

	else if (!strcmp(argv[1], "suspend")) {

		sam.suspend();

	}

	else if (!strcmp(argv[1], "engage")) {
		sam.engage();
	}

	else if (!strcmp(argv[1], "format")) {
		fs_format();
	}

	else if (!strcmp(argv[1], "io") && !strcmp("direction", argv[2])) {
		io.direction(atoi(argv[3]), atoi(argv[4]));

	}

	else if (!strcmp(argv[1], "io") && !strcmp("write", argv[2])) {
		io.write(atoi(argv[3]), atoi(argv[4]));
	}

	return 0;
}


