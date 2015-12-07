#define __private_include__

#include <flipper/flipper.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

#include <platform/at91sam.h>

#include <bme280/bme280.h>

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
	    
	/* ~ Alert the system that the FMR is busy. ~ */
	
	fmr_busy = true;
	
	/* ~ Associate this interrupt with the host target. ~ */
	
	fmr_associate_target(&device);
	
	/* ~ Load a packet from the bus. ~ */
	
	fmr_retrieve();
		
	/* ~ Invoke the FMR. ~ */
	
	fmr_invoke(&device);
	
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

	uint8_t byte = 0xFE;
	
	bme280_configure();
	
//	char buf[64];
	
	while (true) {
		
//		memset(buf, 0, sizeof(buf));
//		
//		float temp = bme280_temperature();
//		
//		sprintf(buf, "Temperature: %f C\n", temp);
//		
//		usart1.push(&buf, strlen(buf));
//		
//		memset(buf, 0, sizeof(buf));
//		
//		float press = bme280_pressure();
//		
//		sprintf(buf, "Pressure: %f Pa\n", press);
//		
//		usart1.push(&buf, strlen(buf));
//		
//		memset(buf, 0, sizeof(buf));
//		
//		float hum = bme280_humidity();
//		
//		sprintf(buf, "Humidity: %f \%\n\n", hum);
//		
//		usart1.push(&buf, strlen(buf));
		
		io.write(8, false);
		
		delay_ms(2000);
		
		io.write(8, true);
		
		delay_ms(2000);
		
	}
	
	return 0;
	
}