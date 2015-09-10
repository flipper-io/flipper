#define __private_include__

#include <flipper/flipper.h>

#include <platform/atmega.h>

#include <fmr/fmr.h>

void __attribute__ ((naked)) __attribute__ ((section(".init8"))) atmega_init(void) {

	/* ~ Clear the WDT reset flag. ~ */
	
	MCUSR &= ~(1 << WDRF);
	
	/* ~ Disable the watchdog timer. ~ */
	
	wdt_disable();
	
	/* ~ Enable global interrupts. ~ */
	
	enable_interrupts();
	
	usart.configure(baudrate(115200));
	
	host_configure(&usart);
	
	memset(fmr_buffer, 0, FLIPPER_PACKET_SIZE);
	
	led.configure();
	
	delay_seconds(5);
	
	led.rgb(0, 16, 0);
	
}

char a = 0;

int main(void) {
	
	while (1) {
		
	}
	
	return 0;
	
}

/* ~ USART recieve interrupt. ~ */

ISR(USART1_RX_vect) {
	
	disable_interrupts();
	
	while (usart0_get_byte() != 0xFE);
	
	for (unsigned i = 0; i < FLIPPER_PACKET_SIZE - 1; i ++) {
		
		fmr_buffer[i] = usart0_get_byte();
		
		usart0_put_byte(fmr_buffer[i]);
		
	}
	
	self_invoke(&host);
	
	enable_interrupts();
	
	reti();
	
}