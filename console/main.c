#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include <fs/crc.h>

#include <flash/flash.h>

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
	
	if (!strcmp(argv[1], "listen")) debug_listen();
	
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
