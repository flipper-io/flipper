#include <flipper.h>
#include <os/scheduler.h>

/* How many clock cycles to wait before giving up initialization. */
#define CLOCK_TIMEOUT 5000

struct _fmr_packet packet;

extern void uart0_put(uint8_t byte);

int debug_putchar(char c, FILE *stream) {
	uart0_put(c);
	return 0;
}

void os_kernel_task(void) {
	while (1) {
		printf("Hello!\n");
		for (int i = 0x1FFFFFC; i > 0; i --) __asm__ __volatile__ ("nop");
	}
}

int main(void) {

	/* Disable the watchdog timer. */
	WDT->WDT_MR = WDT_MR_WDDIS;

	/* Configure the EFC for 5 wait states. */
	EFC0->EEFC_FMR = EEFC_FMR_FWS(PLATFORM_WAIT_STATES);

	/* Configure the primary clock source. */
	if (!(PMC->CKGR_MOR & CKGR_MOR_MOSCSEL)) {
		PMC->CKGR_MOR = CKGR_MOR_KEY_PASSWD | BOARD_OSCOUNT | CKGR_MOR_MOSCRCEN | CKGR_MOR_MOSCXTEN;
		for (uint32_t timeout = 0; !(PMC->PMC_SR & PMC_SR_MOSCXTS) && (timeout ++ < CLOCK_TIMEOUT););
	}

	/* Select external 20MHz oscillator. */
	PMC->CKGR_MOR = CKGR_MOR_KEY_PASSWD | BOARD_OSCOUNT | CKGR_MOR_MOSCRCEN | CKGR_MOR_MOSCXTEN | CKGR_MOR_MOSCSEL;
	for (uint32_t timeout = 0; !(PMC->PMC_SR & PMC_SR_MOSCSELS) && (timeout ++ < CLOCK_TIMEOUT););
	PMC->PMC_MCKR = (PMC->PMC_MCKR & ~(uint32_t)PMC_MCKR_CSS_Msk) | PMC_MCKR_CSS_MAIN_CLK;
	for (uint32_t timeout = 0; !(PMC->PMC_SR & PMC_SR_MCKRDY) && (timeout++ < CLOCK_TIMEOUT););

	/* Configure PLLB as the master clock PLL. */
	PMC->CKGR_PLLBR = BOARD_PLLBR;
	for (uint32_t timeout = 0; !(PMC->PMC_SR & PMC_SR_LOCKB) && (timeout++ < CLOCK_TIMEOUT););

	/* Switch to the main clock. */
	PMC->PMC_MCKR = (BOARD_MCKR & ~PMC_MCKR_CSS_Msk) | PMC_MCKR_CSS_MAIN_CLK;
	for (uint32_t timeout = 0; !(PMC->PMC_SR & PMC_SR_MCKRDY) && (timeout++ < CLOCK_TIMEOUT););
	PMC->PMC_MCKR = BOARD_MCKR;
	for (uint32_t timeout = 0; !(PMC->PMC_SR & PMC_SR_MCKRDY) && (timeout++ < CLOCK_TIMEOUT););

	/* Allow the reset pin to reset the device. */
	RSTC->RSTC_MR = RSTC_MR_KEY_PASSWD | RSTC_MR_URSTEN;

	// extern struct _lf_module adc;
	// extern struct _lf_module button;
	// extern struct _lf_module dac;
	// extern struct _lf_module gpio;
	// extern struct _lf_module i2c;
	// extern struct _lf_module led;
	// extern struct _lf_module pwm;
	// extern struct _lf_module rtc;
	// extern struct _lf_module spi;
	// extern struct _lf_module swd;
	// extern struct _lf_module temp;
	// extern struct _lf_module timer;
	// extern struct _lf_module uart0;
	// extern struct _lf_module usart;
	// extern struct _lf_module usb;
	// extern struct _lf_module wdt;
	
	// dyld_register(&THIS_DEVICE, &adc);
	// dyld_register(&THIS_DEVICE, &button);
	// dyld_register(&THIS_DEVICE, &dac);
	// dyld_register(&THIS_DEVICE, &gpio);
	// dyld_register(&THIS_DEVICE, &i2c);
	// dyld_register(&THIS_DEVICE, &led);
	// dyld_register(&THIS_DEVICE, &pwm);
	// dyld_register(&THIS_DEVICE, &rtc);
	// dyld_register(&THIS_DEVICE, &spi);
	// dyld_register(&THIS_DEVICE, &swd);
	// dyld_register(&THIS_DEVICE, &temp);
	// dyld_register(&THIS_DEVICE, &timer);
	// dyld_register(&THIS_DEVICE, &uart0);
	// dyld_register(&THIS_DEVICE, &usart);
	// dyld_register(&THIS_DEVICE, &usb);
	// dyld_register(&THIS_DEVICE, &wdt);

	/* Enable the FSI pin. */
	gpio_enable(FMR_PIN, 0);
	gpio_write(0, FMR_PIN);

	/* Pull an FMR packet asynchronously to launch FMR. */
	uart0_pull(&packet, sizeof(struct _fmr_packet));
	/* Enable the PDC receive complete interrupt. */
	UART0->UART_IER = UART_IER_ENDRX;

	/* Launch the kernel task. */
	os_scheduler_init();

}

void uart0_isr(void) {

	__disable_irq();

	uint32_t _sr = UART0->UART_SR;

	/* If an entire packet has been received, process it. */
	if (_sr & UART_SR_ENDRX) {
		gpio_write(FMR_PIN, 0);

		UART0->UART_PTCR = UART_PTCR_RXTDIS | UART_PTCR_TXTDIS;

		struct _fmr_result result;
		lf_error_clear();
		fmr_perform(&packet, &result);
		uart0_push(&result, sizeof(struct _fmr_result));
		uart0_pull(&packet, sizeof(struct _fmr_packet));

		/* Wait a bit before raising the FMR pin. */
		for (size_t i = 0; i < 0x3FF; i ++) __asm__ __volatile__("nop");

		gpio_write(0, FMR_PIN);
	} else {
		UART0->UART_CR = UART_CR_RSTSTA;
	}

	__enable_irq();

}
