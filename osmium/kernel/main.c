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
	
	led.configure();
	
	led.rgb(0, 16, 0);
	
	_delay_ms(1);
	
}

char a = 0;

int main(void) {
	
	flash_push("Welcome", 7, 0x1234);
	
	char derp[7];
	
	flash_pull(derp, 7, 0x1234);
	
	usart0_push(derp, 7);
	
	while (1) {
		
		uint8_t packet = usb_receive_packet((void *)(&fmrpacket));
		
		if (packet) self_invoke(&device);
		
	}
	
	return 0;
	
}

/* ~ USART recieve interrupt. ~ */

ISR(USART1_RX_vect) {
	
	disable_interrupts();
	
	while (usart0_get() != 0xFE);
	
	struct _fmr_header *header = &(fmrpacket.header);
	
	/* ~ Load the header of the packet. ~ */
	
	for (unsigned i = 1; i < sizeof(struct _fmr_header); i ++) ((char *)(header))[i] = usart0_get();
	
	/* ~ Load the body of the packet. ~*/
	
	for (unsigned i = 0; i < (header -> length); i ++) ((char *)(&fmrpacket.recipient.object))[i] = usart0_get();
	
	while (usart0_ready()) { (void)usart0_get(); }
		
	enable_interrupts();
	
	self_invoke(&host);
	
}