#define __private_include__

#include <flipper.h>

#include <platform/atmega.h>

#include <fmr/fmr.h>

#include <fs/crc.h>

#include <platform/hid.h>

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
	
}

char a = 0;

int main(void) {
	
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
	
	/* ~ Load the header of the packet. ~ */
	
	for (unsigned i = 1; i < 5; i ++) ((char *)(&fmrpacket))[i] = usart0_get();
	
	for (unsigned i = 0; i < fmrpacket.argc + 3; i ++) ((char *)(&fmrpacket.object))[i] = usart0_get();
	
	while (usart0_ready()) { (void)usart0_get(); }
		
	enable_interrupts();
	
	self_invoke(&device);
	
}