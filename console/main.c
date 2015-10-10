#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

#include "console.h"

#include <fs/crc.h>

#include <flash/flash.h>

#include <fs/tree.h>

int main(int argc, char *argv[]) {
	
	flipper.attach(FLIPPER_SOURCE_USB); //FVM, "/Development/flipper-toolbox/fvm/hal.fvm");
	
	//host_invoke(_io, _io_write, 2, 8, 1);
	
#if 1
	
	if (argc < 2) { printf("Insufficient arguments.\n\n"); return 1; }
	
	else if (!strcmp(argv[1], "erase")) sam_erase_flash();
	
	else if (!strcmp(argv[1], "flash")) { if (argc < 3) { printf("Please provide a path to a firmware file.\n\n"); return 1; } sam_load_firmware(argv[2]); }
	
	else if (!strcmp(argv[1], "ping")) {
		
		usart.push((char []){ 0x80, 0x80, 0x23 }, 3);
		
		char buf[3];
		
		usart.pull(buf, 3);
		
		printf("0x%02X, 0x%02X ,0x%02X\n\n", buf[0], buf[1], buf[2]);
		
	}
	
	else if (!strcmp(argv[1], "reset")) {
		
		sam.reset();
		
	}
	
	else if (!strcmp(argv[1], "test")) {
		
		//usart.put('a');
		
		host.invoke(_button, _button_configure, 0);
		
	}
	
	//uint32_t ip = wifi.ip();
	
	//printf("Wi-Fi with IP address %d.%d.%d.%d\n\n", (uint8_t)(ip), (uint8_t)(ip >> 8), (uint8_t)(ip >> 16), (uint8_t)(ip >> 24));
	
#endif
	
	return 0;
	
}
