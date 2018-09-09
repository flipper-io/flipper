#include "libflipper.h"
#include "os/exceptions.h"

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

    /* cortex stack and reset addresses */
    (isr_t)(&_estack),
    reset_exception,

    /* cortex-m4 exceptions */
    nmi_exception,
    hardfault_exception,
    memmanage_exception,
    busfault_exception,
    usagefault_exception,
    null_isr,
    null_isr,
    null_isr,
    null_isr,
    svc_exception,
    debugmon_exception,
    null_isr,
    pendsv_exception,
    systick_exception,

    /* sam4s specific isrs */
    supc_isr,
    rstc_isr,
    rtc_isr,
    rtt_isr,
    wdt_isr,
    pmc_isr,
    eefc_isr,
    null_isr,
    uart0_isr,
    uart1_isr,
    smc_isr,
    pioa_isr,
    piob_isr,
    pioc_isr,
    usart0_isr,
    usart1_isr,
    usart2_isr,
    null_isr,
    mci_isr,
    twi0_isr,
    twi1_isr,
    spi_isr,
    ssc_isr,
    tc0_isr,
    tc1_isr,
    tc2_isr,
    tc3_isr,
    tc4_isr,
    tc5_isr,
    adc_isr,
    dac_isr,
    pwm_isr,
    crccu_isr,
    acc_isr,
    usbd_isr,
    null_isr
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
    //     SCB->VTOR |= 1 << SCB_VTOR_TBLBASE_Pos ;
    // }

    /* Initialize the C library */
    //__libc_init_array() ;

    /* Branch to the kernel main. */
    main();

    /* Loop if main were ever to return. */
    while (1) {}
}
