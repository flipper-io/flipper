#include <platforms/atsam4s16b.h>

void null_isr(void) {
	while (1);
}

WEAK void nmi_exception(void) {
   while (1);
}

WEAK void hardfault_exception(void) {
   while (1);
}

WEAK void memmanage_exception(void) {
   while (1);
}

WEAK void busfault_exception(void) {
   while (1);
}

WEAK void usagefault_exception(void) {
   while (1);
}

WEAK void svc_exception(void) {
   while (1);
}

WEAK void debugmon_exception(void) {
   while (1);
}

WEAK void pendsv_exception(void) {
   while (1);
}

WEAK void systick_exception(void) {
   while (1);
}

WEAK void supc_isr(void) {
   while (1);
}

WEAK void rstc_isr(void) {
   while (1);
}

WEAK void rtc_isr(void) {
   while (1);
}

WEAK void rtt_isr(void) {
   while (1);
}

WEAK void wdt_isr(void) {
   while (1);
}

WEAK void pmc_isr(void) {
   while (1);
}

WEAK void eefc_isr(void) {
   while (1);
}

WEAK void uart0_isr(void) {
   while (1);
}

WEAK void uart1_isr(void) {
   while (1);
}

WEAK void smc_isr(void) {
   while (1);
}

WEAK void pioa_isr(void) {
   while (1);
}

WEAK void piob_isr(void) {
   while (1);
}

WEAK void pioc_isr(void) {
   while (1);
}

WEAK void usart0_isr(void) {
   while (1);
}

WEAK void usart1_isr(void) {
   while (1);
}

WEAK void usart2_isr(void) {
   while (1);
}

WEAK void mci_isr(void) {
   while (1);
}

WEAK void twi0_isr(void) {
   while (1);
}

WEAK void twi1_isr(void) {
   while (1);
}

WEAK void spi_isr(void) {
   while (1);
}

WEAK void ssc_isr(void) {
   while (1);
}

WEAK void tc0_isr(void) {
   while (1);
}

WEAK void tc1_isr(void) {
   while (1);
}

WEAK void tc2_isr(void) {
   while (1);
}

WEAK void tc3_isr(void) {
   while (1);
}

WEAK void tc4_isr(void) {
   while (1);
}

WEAK void tc5_isr(void) {
   while (1);
}

WEAK void adc_isr(void) {
   while (1);
}

WEAK void dac_isr(void) {
   while (1);
}

WEAK void pwm_isr(void) {
   while (1);
}

WEAK void crccu_isr(void) {
   while (1);
}

WEAK void acc_isr(void) {
   while (1);
}

WEAK void usbd_isr(void) {
   while (1);
}
