/* ----------------------------------------------------------------------------
 *         ATMEL Microcontroller Software Support
 * ----------------------------------------------------------------------------
 * Copyright (c) 2011, Atmel Corporation
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the disclaimer below.
 *
 * Atmel's name may not be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * DISCLAIMER: THIS SOFTWARE IS PROVIDED BY ATMEL "AS IS" AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT ARE
 * DISCLAIMED. IN NO EVENT SHALL ATMEL BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * ----------------------------------------------------------------------------
 */

/*----------------------------------------------------------------------------
 *        Headers
 *----------------------------------------------------------------------------*/

 #include <platform/atsam4s16b.h>

/*----------------------------------------------------------------------------
 *        Exported variables
 *----------------------------------------------------------------------------*/

/* Initialize segments */
extern uint32_t _sfixed;
extern uint32_t _efixed;
extern uint32_t _etext;
extern uint32_t _srelocate;
extern uint32_t _erelocate;
extern uint32_t _szero;
extern uint32_t _ezero;
extern uint32_t _sstack;
extern uint32_t _estack;

/*----------------------------------------------------------------------------
 *        ProtoTypes
 *----------------------------------------------------------------------------*/

/** \cond DOXYGEN_SHOULD_SKIP_THIS */
extern int main( void ) ;
extern void LowLevelInit( void ) ;
/** \endcond */
void ResetException( void ) ;
extern void __libc_init_array( void ) ;

/*------------------------------------------------------------------------------
 *         Exception Table
 *------------------------------------------------------------------------------*/

 typedef void( *IntFunc )( void ) ;

__attribute__((section(".vectors")))
IntFunc exception_table[] = {

	/* Configure Initial Stack Pointer, using linker-generated symbols */
	(IntFunc)(&_estack),
	ResetException,

	nmi_handler,
	hardfault_handler,
	memmanage_handler,
	busfault_handler,
	usagefault_handler,
	0, 0, 0, 0,         /* reserved */
	svc_handler,
	debugmon_handler,
	0,                  /* reserved  */
	pendsv_handler,
	systick_handler,

	/* Configurable interrupts  */
	supc_handler,    /* 0  supply controller */
	rstc_handler,    /* 1  reset controller */
	rtc_handler,     /* 2  real time clock */
	rtt_handler,     /* 3  real time timer */
	wdt_handler,     /* 4  watchdog timer */
	pmc_handler,     /* 5  pmc */
	eefc_handler,    /* 6  eefc */
	null_handler,  /* 7  reserved */
	uart0_handler,   /* 8  uart0 */
	uart1_handler,   /* 9  uart1 */
	smc_handler,     /* 10 smc */
	pioa_handler,    /* 11 parallel io controller a */
	piob_handler,    /* 12 parallel io controller b */
	pioc_handler,    /* 13 parallel io controller c */
	usart0_handler,  /* 14 usart 0 */
	usart1_handler,  /* 15 usart 1 */
	usart2_handler,  /* 16 usart 2 */
	null_handler,  /* 17 reserved */
	mci_handler,     /* 18 mci */
	twi0_handler,    /* 19 twi 0 */
	twi1_handler,    /* 20 twi 1 */
	spi_handler,     /* 21 spi */
	ssc_handler,     /* 22 ssc */
	tc0_handler,     /* 23 timer counter 0 */
	tc1_handler,     /* 24 timer counter 1 */
	tc2_handler,     /* 25 timer counter 2 */
	tc3_handler,     /* 26 timer counter 3 */
	tc4_handler,     /* 27 timer counter 4 */
	tc5_handler,     /* 28 timer counter 5 */
	adc_handler,     /* 29 adc controller */
	dac_handler,     /* 30 dac controller */
	pwm_handler,     /* 31 pwm */
	crccu_handler,   /* 32 crc calculation unit */
	acc_handler,     /* 33 analog comparator */
	usbd_handler,    /* 34 usb device port */
	null_handler   /* 35 not used */
};

/**
 * \brief This is the code that gets called on processor reset.
 * To initialize the device, and call the main() routine.
 */
void ResetException( void )
{
	uint32_t *pSrc, *pDest ;

	/* Low level Initialize */
	LowLevelInit() ;

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

	/* Branch to main function */
	main() ;

	/* Infinite loop */
	while ( 1 ) ;
}
