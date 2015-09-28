#define __private_include__

#include <flipper.h>

#include <platform/atmega.h>

#include <fmr/fmr.h>

#include <fs/crc.h>

#include <platform/hid.h>

#include <usart/usart.h>

void __attribute__ ((naked)) __attribute__ ((section(".init8"))) atmega_init(void) {

	/* ~ Clear the WDT reset flag. ~ */
	
	MCUSR &= ~(1 << WDRF);
	
	/* ~ Disable the watchdog timer. ~ */
	
	wdt_disable();
	
	/* ~ Enable global interrupts. ~ */
	
	enable_interrupts();
	
	usart.configure(baudrate(115200));
	
	host_configure(&usart);
	
	device_configure(&usb);
	
	usb_configure(0);
	
	led.rgb(0, 16, 0);
	
}

extern void usb_receive_interrupt(void);

int main(void) {
	
	flash_push("Welcome", 7, 0x1234);
	
	char derp[7];
	
	flash_pull(derp, 7, 0x1234);
	
	usart0_push(derp, 7);
	
	while (1) {
		
		uint8_t packet = usb_receive_packet((void *)(&fmrpacket));
		
		if (packet) { usb_receive_interrupt(); }
		
	}
	
	return 0;
	
}