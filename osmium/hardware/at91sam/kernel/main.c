#define __private_include__

#include <flipper/flipper.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

#include <platform/at91sam.h>

#include <bme280/bme280.h>

void (* task_to_execute)(void);

void _delay_ms(unsigned long time) {
	
	for (volatile unsigned int i = 0; (i < (F_CPU / 10250) * (time)); i ++);
	
}

void makeitblink(void) {
	
	io.write(8, true);
	
	delay_ms(50);
	
	io.write(8, false);
	
	delay_ms(50);
	
}

bool state = 1;

void usart_interrupt(void) {
	    
	/* ~ Alert the system that the FMR is busy. ~ */
	
	fmr_busy = true;
	
	/* ~ Associate this interrupt with the host target. ~ */
	
	fmr_associate_target(&device);
	
	/* ~ Load a packet from the bus. ~ */
	
	fmr_retrieve();
		
	/* ~ Invoke the FMR. ~ */
	
	fmr_parse(&device);
	
	/* ~ Free the FMR. ~ */
	
	fmr_busy = false;
    
	AT91C_BASE_US0 -> US_CR = AT91C_US_RSTSTA;
	
}

void pio_interrupt() {
	
	state ^= 1;
	
	io.write(8, state);
	
	AT91C_BASE_PIOA -> PIO_ISR;
	
}

int main(void) {
    
	usart.configure((void *)(baudrate(115200)));
	
	usart1.configure((void *)(baudrate(115200)));
	
    spi_configure(0);
    
	at45_configure();
	
    fs_configure();
    
	/* ~ Configure the host for this platform. ~ */
	
	host_configure(&usart);
	
	/* ~ Configure the device for this platform. ~ */ 
	
	device_configure(&usart);
	
	/* ~ IO16 PC Interrupt ~ */
	
	AT91C_BASE_PIOA -> PIO_PER = (1 << 0);
	
	AT91C_BASE_PIOA -> PIO_ODR = (1 << 0);
	
	AT91C_BASE_AIC -> AIC_IDCR = (1 << AT91C_ID_PIOA);
	
	AT91C_BASE_AIC -> AIC_SVR[AT91C_ID_PIOA] = (unsigned)(&pio_interrupt);
	
	AT91C_BASE_AIC -> AIC_SMR[AT91C_ID_PIOA] = AT91C_AIC_SRCTYPE_INT_HIGH_LEVEL | 6;
	
	AT91C_BASE_AIC -> AIC_ICCR = (1 << AT91C_ID_PIOA);
	
	AT91C_BASE_PIOA -> PIO_IER = (1 << 0);
	
	AT91C_BASE_AIC -> AIC_IECR = (1 << AT91C_ID_PIOA);
	
	/* ~ USART Interrupt ~ */
	
	AT91C_BASE_AIC -> AIC_IDCR = (1 << AT91C_ID_US0);
	
	AT91C_BASE_AIC -> AIC_SVR[AT91C_ID_US0] = (unsigned)(&usart_interrupt);
	
	AT91C_BASE_AIC -> AIC_SMR[AT91C_ID_US0] = AT91C_AIC_SRCTYPE_INT_HIGH_LEVEL | 7;
	
	AT91C_BASE_AIC -> AIC_ICCR = (1 << AT91C_ID_US0);
	
	AT91C_BASE_AIC -> AIC_IECR = (1 << AT91C_ID_US0); 
	
	AT91C_BASE_US0 -> US_IER = AT91C_US_RXRDY;
    
	io.configure();
	
	io.direction(8, OUTPUT);
	
	io.write(8, true);
	
	i2c_configure();
	
	/* ~ Load the address of the startup task. ~ */
	
	at45_pull(&task_to_execute, sizeof(uint32_t), config_offset(FDL_CONFIG_BASE, FDL_STARTUP_PROGRAM));
	
	while (true) {
		
		/* ~ Ensure the task is at a valid flash address. ~ */
		
		if ((uint32_t)(task_to_execute) > AT91C_IFLASH && (uint32_t)(task_to_execute) < (AT91C_IFLASH + AT91C_IFLASH_SIZE)) task_to_execute();
		
	}
	
	return 0;
	
}