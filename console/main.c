#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include <fs/crc.h>

#include <flash/flash.h>

int main(int argc, char *argv[]) {
	
	flipper.attach(FLIPPER_SOURCE_USB);
	
	led.rgb(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
	
	char *out = "Lorem ipsum dolor sit amet viverra fusce..sit amet viverra fusce..sit amet viverra fusce..";
	
	host_push(_usart, _usart_push, 0, out, strlen(out));
	
	usart.put('\n');
	
	printf("\n\n");
	
	//char *data = malloc(128);
	
	//memset(data, 0, 128);
	
	//flash.pull(data, strlen(out), 0x1234);
	
	//printf("String: %s\n\n", data);
	
	//uint32_t ip = wifi.ip();
	
	//printf("Wi-Fi with IP address %d.%d.%d.%d\n\n", (uint8_t)(ip), (uint8_t)(ip >> 8), (uint8_t)(ip >> 16), (uint8_t)(ip >> 24));
	
	return 0;
	
}
