#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include "console.h"

#include <fs/crc.h>

#include <flash/flash.h>

#include <fs/tree.h>

typedef void rawhid_t;
void* rawhid_open_only1(int vid, int pid, int usage_page, int usage);
int rawhid_status(rawhid_t *hid);
int rawhid_read(rawhid_t *h, void *buf, int bufsize, int timeout_ms);
int rawhid_write(rawhid_t *hid, const void *buf, int len, int timeout_ms);
void rawhid_close(rawhid_t *h);

void debug_listen(void) {
	
	int num, count;
	
	void *hid;
	
	char buf[64], *in, *out;
	
	hid = rawhid_open_only1(0, 0, 0xFF31, 0x0074);
	
	if (hid == NULL) {
		
		printf("\nCould not find debug stream.\n\n");
		
		exit(EXIT_FAILURE);
		
	}
	
	system("clear");
	
	while (1) {
		
		num = rawhid_read(hid, buf, sizeof(buf), 200);
		
		if (num < 0) break;
		
		if (num == 0) continue;
		
		in = out = buf;
		
		for (count = 0; count < num; count++) {
			
			if (*in) {
				
				*out ++ = *in;
				
			}
			
			in++;
			
		}
		
		count = out - buf;
		
		if (count) {
			
			num = fwrite(buf, 1, count, stdout);
			
			fflush(stdout);
			
		}
		
	}
	
	rawhid_close(hid);
	
	printf("\nDevice disconnected.\n\n");
	
	exit(EXIT_SUCCESS);
	
}

int main(int argc, char *argv[]) {
	
	flipper.attach(FLIPPER_SOURCE_FVM, "/Development/flipper-toolbox/fvm/hal.fvm");
	
	led.rgb(1, 2, 3);
	
#if 0
	
	if (argc < 2) { printf("Insufficient arguments.\n\n"); return 1; }
	
	if (!strcmp(argv[1], "listen")) debug_listen();
	
	else if (!strcmp(argv[1], "erase")) sam_erase_flash();
	
	else if (!strcmp(argv[1], "flash")) { if (argc < 3) { printf("Please provide a path to a firmware file.\n\n"); return 1; } sam_load_firmware(argv[2]); }
	
	else if (!strcmp(argv[1], "test")) {
		
		fs_configure();
		
		fs.format();
		
		fsp _create = flash.alloc(128);
		
		printf("Allocated at 0x%08x\n", _create);
		
	}
	
	//uint32_t ip = wifi.ip();
	
	//printf("Wi-Fi with IP address %d.%d.%d.%d\n\n", (uint8_t)(ip), (uint8_t)(ip >> 8), (uint8_t)(ip >> 16), (uint8_t)(ip >> 24));
	
#endif
	
	return 0;
	
}
