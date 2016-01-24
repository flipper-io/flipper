#define __private_include__

#include <wifi/wifi.h>

#include <fmr/fmr.h>

void wifi_configure(void) {
	
	
	
}

uint32_t wifi_ip(void) {
	
	return device.invoke(_wifi, _wifi_ip, 0);
	
}