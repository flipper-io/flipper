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
	
	uint32_t ip = wifi.ip();
	
	printf("Connected to WiFi with IP address: %i.%i.%i.%i\n\n", (uint8_t)(ip >> 0), (uint8_t)(ip >> 8), (uint8_t)(ip >> 16), (uint8_t)(ip >> 24));
	
	/* ~ Attatch this instance of libflipper to the Flipper Virtual Machine for verbose debugging output. ~ */
	
	flipper.attach(FLIPPER_SOURCE_FVM);
	
	led.rgb(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
	
	return 0;
	
}