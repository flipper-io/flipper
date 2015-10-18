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
	
	//fs_configure();
	
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
	
	else if (!strcmp(argv[1], "read")) {
		
		size_t len = atoi(argv[2]);
		
		char *mem = malloc(len);
		
		bzero(mem, len);
		
		fsp src = atoi(argv[3]);
		
		flash.pull(mem, len, src);
		
		//printf("\nRead %s\n\n", mem);
		
		printf("\nReading from 0x%08x\n\n", src);
		
		for (int i = 0; i < len; i ++) printf("0x%02x ", mem[i]);
		
		printf("\n\n");
		
	}
	
	else if (!strcmp(argv[1], "root")) {
		
		fs.configure();
		
		printf("\nRoot at 0x%08x, Brk at 0x%08x, Free at 0x%08x\n\n", _root_leaf, _break_value, _free_list);
		
	}
	
	else if (!strcmp(argv[1], "write")) {
		
		size_t len = strlen(argv[2]);
		
		fsp dest = atoi(argv[3]);
		
		flash.push(argv[2], len, dest);
		
	}
	
	else if (!strcmp(argv[1], "eraseflash")) {
		
		flash.format();
		
	}
	
	else if (!strcmp(argv[1], "alloc")) {
		
		fsp addr = flash.alloc(atoi(argv[2]));
		
		printf("\nAllocated at 0x%08X (%u)\n\n", addr, addr);
		
	}
	
	else if (!strcmp(argv[1], "free")) {
		
		fsp addr = atoi(argv[2]);
		
		flash.free(addr);
		
		printf("\nFree'd 0x%08X\n\n", addr);
		
	}
	
	else if (!strcmp(argv[1], "format")) {
		
		fs.format();
		
	}
	
	else if (!strcmp(argv[1], "spi")) {

		host.invoke(_spi, _spi_enable, 0);
		
		host.invoke(_spi, _spi_put, 2, little('a'), 0);
		
		host.invoke(_spi, _spi_disable, 0);
		
	}
	
	else if (!strcmp(argv[1], "readflash")) {
		
		host.invoke(_button, _button_read, 2, 0, 0);
		
	}
	
	else if (!strcmp(argv[1], "io") && !strcmp("direction", argv[2])) {
		
		host.invoke(_io, _io_set_direction, 4, little(atoi(argv[3])), 0, little(OUTPUT), 0);
		
	}
	
	else if (!strcmp(argv[1], "io") && !strcmp("write", argv[2])) {
		
		host.invoke(_io, _io_write, 4, little(atoi(argv[3])), 0, little(!atoi(argv[4])), 0);
		
	}
	
	else {
		
		printf("\nError!\n\n");
		
	}
	
	//uint32_t ip = wifi.ip();
	
	//printf("Wi-Fi with IP address %d.%d.%d.%d\n\n", (uint8_t)(ip), (uint8_t)(ip >> 8), (uint8_t)(ip >> 16), (uint8_t)(ip >> 24));
	
	return 0;
	
}
