#define __private_include__

#include <flipper/flipper.h>

#include <platform/atmega.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

#include <fs/crc.h>

#include <platform/hid.h>

#include <usart/usart.h>

#include <platform/power.h>

extern void libflipper_init(void);

void __attribute__ ((naked)) __attribute__ ((section(".init8"))) atmega_init(void) {
	
	/* ~ Clear the WDT reset flag. ~ */
	
	MCUSR &= ~(1 << WDRF);
	
	/* ~ Disable the watchdog timer. ~ */
	
	wdt_disable();
	
	/* ~ Enable global interrupts. ~ */
	
	disable_interrupts();
	
	/* ~ Initialize the drivers. ~ */
	
	libflipper_init();
	
	/* ~ Configure the USART bus. ~ */
	
	usart0_configure((void *)(baudrate(115200)));
	
	/* ~ Configure the host for this platform. ~ */
	
	host_configure(&usb);
	
	/* ~ Configure the device for this platform. ~ */
	
	device_configure(&usart);
	
	/* ~ Configure the filesystem. ~ */
	
	fs_configure();
	
	/* ~ Wait for the computer to attach. ~ */
	
	delay_ms(250);
	
	/* ~ Throw on the blue LED to indicate that we are waiting for a host PC to attach. ~ */
	
	led.rgb(0, 0, 16);
	
	enable_interrupts();
	
	/* ~ Configure USB. ~ */
	
	usb_configure(0);
	
	/* ~ Light the status LED to indicate successful configuration. ~ */
	
	led.rgb(0, 16, 0);
	
}

extern void usb_receive_interrupt(void);

int main(void) {
	
	while (1) {
		
		while (!fmr_busy) {
			
			uint8_t packet = usb_receive_packet((void *)(&fmrpacket));
			
			if (packet) { usb_receive_interrupt(); }
			
		}
		
	}
	
	return 0;
	
}