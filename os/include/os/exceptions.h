#ifndef __exceptions_h__
#define __exceptions_h__

typedef void (*isr_t)(void);

/* Null handler. */
extern void null_isr(void);

/* Cortex-M4 exception handlers. */
extern void reset_exception(void);
extern void nmi_exception(void);
extern void hardfault_exception(void);
extern void memmanage_exception(void);
extern void busfault_exception(void);
extern void usagefault_exception(void);
extern void svc_exception(void);
extern void debugmon_exception(void);
extern void pendsv_exception(void);
extern void systick_exception(void);

/* Peripheral interrupt service routines. */
extern void acc_isr(void);
extern void adc_isr(void);
extern void crccu_isr(void);
extern void dac_isr(void);
extern void eefc_isr(void);
extern void mci_isr(void);
extern void pioa_isr(void);
extern void piob_isr(void);
extern void pioc_isr(void);
extern void pmc_isr(void);
extern void pwm_isr(void);
extern void rstc_isr(void);
extern void rtc_isr(void);
extern void rtt_isr(void);
extern void smc_isr(void);
extern void spi_isr(void);
extern void ssc_isr(void);
extern void supc_isr(void);
extern void tc0_isr(void);
extern void tc1_isr(void);
extern void tc2_isr(void);
extern void tc3_isr(void);
extern void tc4_isr(void);
extern void tc5_isr(void);
extern void twi0_isr(void);
extern void twi1_isr(void);
extern void uart0_isr(void);
extern void uart1_isr(void);
extern void usart0_isr(void);
extern void usart1_isr(void);
extern void usart2_isr(void);
extern void usbd_isr(void);
extern void wdt_isr(void);

#endif
