#include <servo.h>

#include "atmel.h"

void servo_configure(void) {
	
	AT91C_BASE_PIOA->PIO_PDR |= (1 << 0x00);
	AT91C_BASE_PIOA->PIO_ASR |= (1 << 0x00);
	
	//Disable IO on the pin for our pwm.
	AT91C_BASE_PIOA->PIO_PDR |= AT91C_PA0_PWM0;
	
	//Assign P0 (board pin 16) to peripheral A, which is PWM0.
	AT91C_BASE_PIOA->PIO_ASR |= (0x1 << 0);
	
	//Power on the pwm controller.
	AT91C_BASE_PMC->PMC_PCER |= (1 << AT91C_ID_PWMC);
	
	//Diable all PWM channels
	AT91C_BASE_PWMC->PWMC_DIS = AT91C_PWMC_CHID0 |
	AT91C_PWMC_CHID1 |
	AT91C_PWMC_CHID2 |
	AT91C_PWMC_CHID3;
	
	//Choose a pwm channel to use.
	AT91S_PWMC_CH *pwm = AT91C_BASE_PWMC_CH0;
	
	//Set the pwm channel's clock to MCK/32.
	pwm->PWMC_CMR |= 0x5;
	
	//Set the polarity to start high.
	pwm->PWMC_CMR |= AT91C_PWMC_CPOL;
	
	//Set the channel period to 20ms.
	pwm->PWMC_CPRDR = 30034;
	
	//Set the channel duty cycle to half the period.
	pwm->PWMC_CDTYR = 1500;
	
	//Enable pwm channel 0.
	AT91C_BASE_PWMC->PWMC_ENA = AT91C_PWMC_CHID0;
	
}

void servo_attach(uint8_t pin) {
	
	
	
}

void servo_rotate(uint32_t position) {
	
	AT91C_BASE_PWMC_CH0->PWMC_CMR &= ~AT91C_PWMC_CPD;
	
	AT91C_BASE_PWMC_CH0->PWMC_CUPDR = position;
	
}