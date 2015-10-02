#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include <fs/crc.h>

#include <flash/flash.h>

int main(int argc, char *argv[]) {
	
	flipper.attach(FLIPPER_SOURCE_NETWORK, "129.21.81.189");
	
	led.rgb(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
	
	//uint32_t ip = wifi.ip();
	
	//printf("Wi-Fi with IP address %d.%d.%d.%d\n\n", (uint8_t)(ip), (uint8_t)(ip >> 8), (uint8_t)(ip >> 16), (uint8_t)(ip >> 24));
	
	return 0;
	
}