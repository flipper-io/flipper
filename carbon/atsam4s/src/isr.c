#include "libflipper.h"

void null_isr(void) {
	while (1) __asm__ __volatile__ ("nop");
}

LF_WEAK void nmi_exception(void) {
   while (1);
}

LF_WEAK void hardfault_exception(void) {
   while (1);
}

LF_WEAK void memmanage_exception(void) {
   while (1);
}

LF_WEAK void busfault_exception(void) {
   while (1);
}

LF_WEAK void usagefault_exception(void) {
   while (1);
}

LF_WEAK void svc_exception(void) {
   while (1);
}

LF_WEAK void debugmon_exception(void) {
   while (1);
}

LF_WEAK void pendsv_exception(void) {
   while (1);
}

LF_WEAK void systick_exception(void) {
   while (1);
}

LF_WEAK void supc_isr(void) {
   while (1);
}

LF_WEAK void rstc_isr(void) {
   while (1);
}

LF_WEAK void rtc_isr(void) {
   while (1);
}

LF_WEAK void rtt_isr(void) {
   while (1);
}

LF_WEAK void wdt_isr(void) {
   while (1);
}

LF_WEAK void pmc_isr(void) {
   while (1);
}

LF_WEAK void eefc_isr(void) {
   while (1);
}

LF_WEAK void uart0_isr(void) {
   while (1);
}

LF_WEAK void uart1_isr(void) {
   while (1);
}

LF_WEAK void smc_isr(void) {
   while (1);
}

LF_WEAK void pioa_isr(void) {
   while (1);
}

LF_WEAK void piob_isr(void) {
   while (1);
}

LF_WEAK void pioc_isr(void) {
   while (1);
}

LF_WEAK void usart0_isr(void) {
   while (1);
}

LF_WEAK void usart1_isr(void) {
   while (1);
}

LF_WEAK void usart2_isr(void) {
   while (1);
}

LF_WEAK void mci_isr(void) {
   while (1);
}

LF_WEAK void twi0_isr(void) {
   while (1);
}

LF_WEAK void twi1_isr(void) {
   while (1);
}

LF_WEAK void spi_isr(void) {
   while (1);
}

LF_WEAK void ssc_isr(void) {
   while (1);
}

LF_WEAK void tc0_isr(void) {
   while (1);
}

LF_WEAK void tc1_isr(void) {
   while (1);
}

LF_WEAK void tc2_isr(void) {
   while (1);
}

LF_WEAK void tc3_isr(void) {
   while (1);
}

LF_WEAK void tc4_isr(void) {
   while (1);
}

LF_WEAK void tc5_isr(void) {
   while (1);
}

LF_WEAK void adc_isr(void) {
   while (1);
}

LF_WEAK void dac_isr(void) {
   while (1);
}

LF_WEAK void pwm_isr(void) {
   while (1);
}

LF_WEAK void crccu_isr(void) {
   while (1);
}

LF_WEAK void acc_isr(void) {
   while (1);
}

LF_WEAK void usbd_isr(void) {
   while (1);
}
