#include <flipper.h>
#include <os/exceptions.h>

extern uint32_t _sfixed;
extern uint32_t _efixed;
extern uint32_t _etext;
extern uint32_t _srelocate;
extern uint32_t _erelocate;
extern uint32_t _szero;
extern uint32_t _ezero;
extern uint32_t _sstack;
extern uint32_t _estack;

volatile bool g_interrupt_enabled;

__attribute__((section(".vectors"))) isr_t vector_table[] = {

	(isr_t)(&_estack),
	reset_exception,

	nmi_exception,
	hardfault_exception,
	memmanage_exception,
	busfault_exception,
	usagefault_exception,
	NULL,
	NULL,
	NULL,
	NULL,
	svc_exception,
	debugmon_exception,
	NULL,
	pendsv_exception,
	systick_exception,

	supc_isr,    /* 0  supply controller */
	rstc_isr,    /* 1  reset controller */
	rtc_isr,     /* 2  real time clock */
	rtt_isr,     /* 3  real time timer */
	wdt_isr,     /* 4  watchdog timer */
	pmc_isr,     /* 5  pmc */
	eefc_isr,    /* 6  eefc */
	null_isr,    /* 7  reserved */
	uart0_isr,   /* 8  uart0 */
	uart1_isr,   /* 9  uart1 */
	smc_isr,     /* 10 smc */
	pioa_isr,    /* 11 parallel io controller a */
	piob_isr,    /* 12 parallel io controller b */
	pioc_isr,    /* 13 parallel io controller c */
	usart0_isr,  /* 14 usart 0 */
	usart1_isr,  /* 15 usart 1 */
	usart2_isr,  /* 16 usart 2 */
	null_isr,    /* 17 reserved */
	mci_isr,     /* 18 mci */
	twi0_isr,    /* 19 twi 0 */
	twi1_isr,    /* 20 twi 1 */
	spi_isr,     /* 21 spi */
	ssc_isr,     /* 22 ssc */
	tc0_isr,     /* 23 timer counter 0 */
	tc1_isr,     /* 24 timer counter 1 */
	tc2_isr,     /* 25 timer counter 2 */
	tc3_isr,     /* 26 timer counter 3 */
	tc4_isr,     /* 27 timer counter 4 */
	tc5_isr,     /* 28 timer counter 5 */
	adc_isr,     /* 29 adc controller */
	dac_isr,     /* 30 dac controller */
	pwm_isr,     /* 31 pwm */
	crccu_isr,   /* 32 crc calculation unit */
	acc_isr,     /* 33 analog comparator */
	usbd_isr,    /* 34 usb device port */
	null_isr     /* 35 not used */
};

extern void system_init(void);
extern int main(void);

void reset_exception(void) {

	uint32_t *pSrc, *pDest ;

	/* Initialize the relocate segment */
	pSrc = &_etext ;
	pDest = &_srelocate ;

	if ( pSrc != pDest )
	{
		for ( ; pDest < &_erelocate ; )
		{
			*pDest++ = *pSrc++ ;
		}
	}

	/* Clear the zero segment */
	for ( pDest = &_szero ; pDest < &_ezero ; )
	{
		*pDest++ = 0;
	}

	/* Set the vector table base address */
	// pSrc = (uint32_t *)&_sfixed;
	// SCB->VTOR = ( (uint32_t)pSrc & SCB_VTOR_TBLOFF_Msk ) ;
	//
	// if ( ((uint32_t)pSrc >= IRAM_ADDR) && ((uint32_t)pSrc < IRAM_ADDR+IRAM_SIZE) )
	// {
	// 	SCB->VTOR |= 1 << SCB_VTOR_TBLBASE_Pos ;
	// }

	/* Initialize the C library */
	//__libc_init_array() ;

	/* Branch to the kernel main. */
	main();

	/* Loop if main were ever to return. */
	while (1);
}
