#define __private_include__

#include <wifi/wifi.h>

#include <usart/usart.h>

#include <platform/atmega.h>

void wifi_configure(void) {
	
	
	
}

uint32_t wifi_ip(void) {
	
	uint32_t ip;
	
	disable_interrupts();
	
	usart.put('a');
	
	usart.pull(&ip, sizeof(uint32_t));
	
	enable_interrupts();
	
	return ip;
	
}