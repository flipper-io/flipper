#define __private_include__
#include <flipper/flipper.h>
#include "console.h"
#include <flipper/fs/crc.h>
#include <unistd.h>

int main(int argc, char *argv[]) {

	if (argc < 2) {
		printf("\nUsage: flipper [install | io | direction | flash]\n\n");
		return EXIT_SUCCESS;
	}

	/* ~ Attatch this instance of libflipper to the first device present over USB. ~ */
	flipper.attach();

	if (!strcmp(argv[1], "flash")) {
		sam_load_firmware(argv[2]);
	}

	if (!strcmp(argv[1], "install")) {

		if (argc < 4) { printf("\nPlease specify a bundle identifer ('io.flipper.module') as well as the name of the module.\n\n"); return EXIT_FAILURE; }

		/* ~ Parse user input from the variadic argument list. ~ */
		char *bundle = argv[2], *path = argv[3];

		sam.power(0);

		usleep(10000);

		fs.format();

		usleep(10000);

		/* ~ Send the file to the device. ~ */
		fs.upload(path, bundle);

		usleep(10000);

		sam.power(1);

		/* ~ Let the operating system boot. ~ */
		usleep(100000);

		uint16_t key = checksum(bundle, strlen(bundle));

		/* ~ Instruct the dynamic loader to load the file. ~ */
		fdl.load(key);

	}

	else if (!strcmp(argv[1], "launch")) {

		char *bundle = argv[2];
		uint16_t key = checksum(bundle, strlen(bundle));

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


