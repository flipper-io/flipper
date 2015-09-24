#define __private_include__

#include <flipper.h>



#include <fmr/fmr.h>

#include <fs/crc.h>

int main(int argc, char *argv[]) {
	
	// if (argc < 2) { printf("Welcome to the Flipper Console.\n\n"); }
	
	if (argc < 5) { printf("Insufficient arguments.\n\n"); return 1; }
	
	if (!strcmp(argv[4], "u")) flipper.attach(FLIPPER_SOURCE_USB);
	
	else if (!strcmp(argv[4], "n")) flipper.attach(FLIPPER_SOURCE_NETWORK, "129.21.80.149");
	
	else {
		
		printf("NO VALID DISPATCH LOCATION.\n\n");
		
		return 1;
		
	}
	
	led.rgb(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
	
	/* ~ Attatch this instance of libflipper to the Flipper Virtual Machine for verbose debugging output. ~ */
	
	flipper.attach(FLIPPER_SOURCE_FVM);
	
	led.rgb(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
	
	return 0;
	
	for (int i = 0; i <= 32; i ++) {
		
		uint16_t cs = checksum((void *)(&fmrpacket) + offsetof(struct _fmr_packet, object), i);
		
		if (cs == 0x3A55 || cs == 0x553A) { printf("Success at %i", i); }
		
		else { printf("No success at %i. Got %04X.\n", i, cs); }
		
	}
	
}