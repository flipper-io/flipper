#define __private_include__

#include <flipper/flipper.h>

/* ~ Platform specific include directives. ~ */

#include <platform/atmega.h>

#include <platform/hid.h>

extern void usb_receive_interrupt(void);

int main(void) {
	
	
	/* -- PLATFORM SPECIFIC INITIALIZATION -- */
	
	
#pragma message("Perhaps engaging the WDT would be a good idea. In the event of a timeout, the device can reset, throw a warning, and continue.")
	
	/* ~ Clear the WDT reset flag. ~ */
	
	MCUSR &= ~(1 << WDRF);
	
	/* ~ Disable the watchdog timer. ~ */
	
	wdt_disable();
	
	
	/* -- PLATFORM INSPECIFIC INITIALIZATION -- */
	
	
	/* ~ Configure the filesystem and its dependencies. The order to this is important. ~ */
	
	at45_configure();
	
	spi_configure(0);
	
	fs_configure();
	
	/* ~ Configure the peripherals. ~ */
	
	button_configure();
	
	led_configure();
	
	io_configure();
	
	sam_configure();
	
	wifi_configure();
	
	i2c_configure();
	
#pragma message("Both of these are initialized here for legacy reasons. They should be converted to passthroughs.")
	
	timer_configure();
	
	pwm_configure();
	
	/* ~ Configure the builtins. ~ */
	
	error_configure();
	
	// fdl_configure();
	
	fmr_configure();
	
	/* ~ Configure the busses. ~ */
	
	usart0_configure((void *)(baudrate(115200)));
	
	usb_configure(0);
	
	
	/* -- FLIPPER MESSAGE RUNTIME INITIALIZATION -- */
	
	
	/* ~ Configure the host for this platform. ~ */
	
	host_configure(&usb);
	
	/* ~ Configure the device for this platform. ~ */
	
	device_configure(&usart);
	
	
	/* -- USER INTERFACE -- */
	
	
	/* ~ Wait for the computer to attach. ~ */
	
	delay_ms(250);
	
	/* ~ Light the status LED to indicate successful configuration. ~ */
	
	led.rgb(0, 16, 0);
	
	
	/* -- SCHEDULER -- */
	
	
	while (1) {
		
#pragma message("This approach is trash. Register a callback in vect10, the USB communication interrupt. ~ */")
		
		uint8_t packet = usb_receive_packet((void *)(&fmrpacket));
		
		if (packet) { usb_receive_interrupt(); }
		
	}
	
	return 0;
	
}