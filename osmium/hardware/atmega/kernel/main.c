#define __private_include__

#include <flipper.h>

#include <platform/atmega.h>

#include <fmr/fmr.h>

#include <fs/crc.h>

#include <platform/hid.h>

#include <usart/usart.h>

#include <platform/power.h>

void __attribute__ ((naked)) __attribute__ ((section(".init8"))) atmega_init(void) {

	/* ~ Clear the WDT reset flag. ~ */
	
	MCUSR &= ~(1 << WDRF);
	
	/* ~ Disable the watchdog timer. ~ */
	
	wdt_disable();
	
	/* ~ Enable global interrupts. ~ */
	
	enable_interrupts();

#if false
	
	/* If the U2 is scheduled to power on after a reset, continue initialization. */
	
	if (power_on_scheduled()) { u2_power_on = true; u2_power_on_after_reset(false); }
	
	/* Otherwise, send the U2 into sleep mode until the power button is pressed. */
	
	else { u2_power_on = false; flipper_sleep(); }

#endif
	
	/* ~ Configure the USART bus. ~ */
	
	usart0_configure((void *)(baudrate(115200)));
	
	/* ~ Enable the USART interrupt. ~ */
	
	usart0_enable();
	
	/* ~ Configure the host for this platform. ~ */
	
	host_configure(&usb);
	
	/* ~ Configure the device for this platform. ~ */
	
	device_configure(&usart);
	
	//fs_configure();
	
	/* ~ Configure USB. ~ */
	
	usb_configure(0);
	
	/* ~ Light the status LED to indicate successful configuration. ~ */
	
	led.rgb(0, 16, 0);
	
}

uint8_t colors = 21;

ISR(TIMER1_COMPA_vect) {
	
	led_set_rgb((colors & bit(2)), (colors & bit(3)), (colors & bit(4)));
	
	colors <<= 1;
	
	if (colors == 168) colors = 21;
	
}

void enable_timer(void) {
	
	TCCR1B |= (1 << WGM12);
	
	TIMSK1 |= (1 << OCIE1A);
	
	OCR1A = 20833;
	
	TCCR1B |= ((1 << CS10) | (1 << CS11));
	
}

extern void usb_receive_interrupt(void);

int main(void) {
	
	//enable_timer();
	
	//flash_push("Welcome", 7, 0x1234);
	
	//char derp[7];
	
	//flash_pull(derp, 7, 0x1234);
	
	//usart0_push(derp, 7);
	
	while (1) {
		
		while (!fmr_busy) {
			
			uint8_t packet = usb_receive_packet((void *)(&fmrpacket));
			
			if (packet) { usb_receive_interrupt(); }
			
		}
		
	}
	
	return 0;
	
}