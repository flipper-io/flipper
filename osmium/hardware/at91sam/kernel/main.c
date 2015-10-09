#define __private_include__

#include <flipper/flipper.h>

#include <platform/at91sam.h>

void _delay_ms(unsigned long time) {
	
	for (volatile unsigned int i = 0; (i < (F_CPU / 10250) * (time)); i ++);
	
}

void makeitblink(void) {
	
	io.write(8, ON);
	
	delay_ms(50);
	
	io.write(8, OFF);
	
	delay_ms(50);
	
}

bool state = 1;

void usart_interrupt(void) {
	
	state ^= 1;
	
	io.write(8, state);
	
	usart.put(usart.get());
	
}

void pio_interrupt() {
	
	state ^= 1;
	
	io.write(8, state);
	
	AT91C_BASE_PIOA -> PIO_ISR;
	
}

int main(void) {
	
	io.configure();
	
	io.direction(8, OUTPUT);
	
	io.write(8, state);
	
	usart.configure((void *)(baudrate(115200)));
	
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
	
	AT91C_BASE_PIOA -> PIO_IER = (1 << 7);
	
	AT91C_BASE_AIC -> AIC_IECR = (1 << AT91C_ID_PIOA);
	
	AT91C_BASE_AIC -> AIC_IDCR = (1 << AT91C_ID_US0);
	
	AT91C_BASE_AIC -> AIC_SVR[AT91C_ID_US0] = (unsigned)(&usart_interrupt);
	
	AT91C_BASE_AIC -> AIC_SMR[AT91C_ID_US0] = AT91C_AIC_SRCTYPE_INT_HIGH_LEVEL | 7;
	
	AT91C_BASE_AIC -> AIC_ICCR = (1 << AT91C_ID_US0);
	
	AT91C_BASE_AIC -> AIC_IECR = (1 << AT91C_ID_US0); 
	
	AT91C_BASE_US0 -> US_IER = AT91C_US_RXRDY;
	
	while (true);
	
	return 0;
	
}