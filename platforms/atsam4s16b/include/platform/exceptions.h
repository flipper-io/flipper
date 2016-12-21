#ifndef __exceptions_h__
#define __exceptions_h__

/* Null handler. */
extern void null_handler(void);

/* Cortex-M4 exception handlers. */
extern void nmi_handler(void);
extern void hardfault_handler(void);
extern void memmanage_handler(void);
extern void busfault_handler(void);
extern void usagefault_handler(void);
extern void svc_handler(void);
extern void debugmon_handler(void);
extern void pendsv_handler(void);
extern void systick_handler(void);

/* Peripheral exception handlers. */
extern void acc_handler(void);
extern void adc_handler(void);
extern void crccu_handler(void);
extern void dac_handler(void);
extern void eefc_handler(void);
extern void mci_handler(void);
extern void pioa_handler(void);
extern void piob_handler(void);
extern void pioc_handler(void);
extern void pmc_handler(void);
extern void pwm_handler(void);
extern void rstc_handler(void);
extern void rtc_handler(void);
extern void rtt_handler(void);
extern void smc_handler(void);
extern void spi_handler(void);
extern void ssc_handler(void);
extern void supc_handler(void);
extern void tc0_handler(void);
extern void tc1_handler(void);
extern void tc2_handler(void);
extern void tc3_handler(void);
extern void tc4_handler(void);
extern void tc5_handler(void);
extern void twi0_handler(void);
extern void twi1_handler(void);
extern void uart0_handler(void);
extern void uart1_handler(void);
extern void usart0_handler(void);
extern void usart1_handler(void);
extern void usart2_handler(void);
extern void usbd_handler(void);
extern void wdt_handler(void);

#endif
