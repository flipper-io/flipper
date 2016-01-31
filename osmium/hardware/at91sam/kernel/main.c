#define __private_include__

#include <flipper/flipper.h>

#include <fmr/fmr.h>

#include <platform/at91sam.h>

#include <platform/scheduler.h>

/* ~ Garbage delay function. ~ */

/* ~ PLEASE MOVE ME AND REPLACE ME! ~ */

void _delay_ms(unsigned long time) {
	
	for (volatile unsigned int i = 0; (i < (F_CPU / 10250) * (time)); i ++);
	
}

/* ~ This function is the interrupt service routine that is called when a character is received over USART. ~ */

/* ~ MOVE ME! ~ */

void usart_interrupt(void) {
	
	/* ~ Associate this interrupt with the host target. ~ */
	
	fmr_associate_target(&device);
	
	/* ~ Load a packet from the bus. ~ */
	
	fmr_retrieve();
		
	/* ~ Invoke the FMR. ~ */
	
	fmr_parse(&device);
	
	/* ~ Reset the USART hardware to prepare for the next incoming FMR packet. ~ */
    
	AT91C_BASE_US0 -> US_CR = AT91C_US_RSTSTA;
	
}

/* ~ This is the entry point of the operating system kernel. ~ */

int main(void) {
	
	
	usart1_configure((void *)(baudrate(115200)));
	
	
	/* -- PLATFORM INSPECIFIC INITIALIZATION -- */
	
	
	/* ~ Configure the filesystem and its dependencies. The order to this is important. ~ */
	
	at45_configure();
	
	spi_configure(0);
	
	spi_enable();
	
	fs_configure();
	
	/* ~ Configure the peripherals. ~ */
	
	button_configure();
	
	led_configure();
	
	io_configure();
	
	timer_configure();
	
	pwm_configure();
	
	sam_configure();
	
	wifi_configure();
	
	i2c_configure();
	
	/* ~ Configure the builtins. ~ */
	
	error_configure();
	
	fdl_configure();
	
	fmr_configure();
	
	/* ~ Configure the busses. ~ */
	
	usart0_configure((void *)(baudrate(115200)));
	
	usb_configure(0);
	

	/* -- FLIPPER MESSAGE RUNTIME INITIALIZATION -- */
	
	
	/* ~ Perform platform specific initializations that pertain to the Flipper Message Runtime. ~ */
	
	host_configure(&usart);
	
	device_configure(&usart);
	
	/* ~ Register the USART interrupt with a callback to the appropriate handler. ~ */
	
#pragma message("The behavior of this callback is unknown when multiple characters are received during the interrupt event.")
	
	AT91C_BASE_AIC -> AIC_IDCR = (1 << AT91C_ID_US0);
	
	AT91C_BASE_AIC -> AIC_SVR[AT91C_ID_US0] = (unsigned)(&usart_interrupt);
	
	AT91C_BASE_AIC -> AIC_SMR[AT91C_ID_US0] = AT91C_AIC_SRCTYPE_INT_HIGH_LEVEL | 7;
	
	AT91C_BASE_AIC -> AIC_ICCR = (1 << AT91C_ID_US0);
	
	AT91C_BASE_AIC -> AIC_IECR = (1 << AT91C_ID_US0); 
	
	AT91C_BASE_US0 -> US_IER = AT91C_US_RXRDY;

	
//	while (false) {
//		
//		delay_ms(250);
//		
//		io.write(7, state);
//		
//		state ^= 1;
//		
//	}
	
	/* -- SCHEDULER -- */
	
	begin_scheduling();
	
	return 0;
	
}

/*

	~ UNIMPLEMENTED CODE ~

	AT91C_BASE_PIOA -> PIO_PER = (1 << 0);

	AT91C_BASE_PIOA -> PIO_ODR = (1 << 0);

	AT91C_BASE_AIC -> AIC_IDCR = (1 << AT91C_ID_PIOA);

	AT91C_BASE_AIC -> AIC_SVR[AT91C_ID_PIOA] = (unsigned)(&pio_interrupt);

	AT91C_BASE_AIC -> AIC_SMR[AT91C_ID_PIOA] = AT91C_AIC_SRCTYPE_INT_HIGH_LEVEL | 6;

	AT91C_BASE_AIC -> AIC_ICCR = (1 << AT91C_ID_PIOA);

	AT91C_BASE_PIOA -> PIO_IER = (1 << 0);

	AT91C_BASE_AIC -> AIC_IECR = (1 << AT91C_ID_PIOA);

*/