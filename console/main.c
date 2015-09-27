#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include <fs/crc.h>

#include <flash/flash.h>

int main(int argc, char *argv[]) {
	
#if 0
	
	// if (argc < 2) { printf("Welcome to the Flipper Console.\n\n"); }
	
	if (argc < 5) { printf("Insufficient arguments.\n\n"); return 1; }
	
	if (!strcmp(argv[4], "u")) flipper.attach(FLIPPER_SOURCE_USB);
	
	else if (!strcmp(argv[4], "n")) flipper.attach(FLIPPER_SOURCE_NETWORK, "129.21.80.56");
	
	else if (!strcmp(argv[4], "v")) flipper.attach(FLIPPER_SOURCE_FVM);
	
	else {
		
		printf("NO VALID DISPATCH LOCATION.\n\n");
		
		return 1;
		
	}

/*
	
	led.rgb(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
	
	uint32_t ip = wifi.ip();
	
	printf("Connected to WiFi with IP address: %i.%i.%i.%i\n\n", (uint8_t)(ip >> 0), (uint8_t)(ip >> 8), (uint8_t)(ip >> 16), (uint8_t)(ip >> 24));
	
 */
 
	/* ~ Attatch this instance of libflipper to the Flipper Virtual Machine for verbose debugging output. ~ */
	
#endif
	
	flipper.attach(FLIPPER_SOURCE_USB);//NETWORK, "129.21.80.56");
	
	char *data = "Lorem ipsum dolor sit amet, consectetur";
	
	printf("This is %lu bytes\n", strlen(data));
	
	//char *derp = flash.dereference(0x1234, 11);
	
	//device.pull(_flash, _flash_pull, 2, rec, 11, 0, 0x1234);
	
	//printf("Read from flash: %s\n\n", derp);
	
	//free(derp);
	
	usart.put('b');
	
	for (int i = 0; i < 10000; i ++) {
	
		usart.push(data, strlen(data));
		
	}
	
	return 0;
	
}