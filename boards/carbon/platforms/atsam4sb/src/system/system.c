#define __private_include__
#include <flipper/carbon/modules/gpio.h>
#include <flipper/carbon/modules/uart0.h>
#include <flipper/carbon/modules.h>
#include <flipper/carbon/platforms/atsam4s16b.h>

/* The fmr_device object containing global state about this device. */
struct _lf_device lf_self = {
	{
		"flipper",
		0xc713,
		LF_VERSION,
		(lf_device_32bit | lf_device_little_endian)
	},
	NULL,
	E_OK
};

/* Experimental: Entry address of the loaded program. */
volatile void *application_entry = NULL;

void uart0_pull_wait(void *destination, lf_size_t length) {
	/* Disable the PDC receive complete interrupt. */
	UART0 -> UART_IDR = UART_IDR_ENDRX;
	/* Set the transmission length and destination pointer. */
	UART0 -> UART_RCR = length;
	UART0 -> UART_RPR = (uintptr_t)(destination);
	/* Enable the receiver. */
	UART0 -> UART_PTCR = UART_PTCR_RXTEN;
	/* Wait until the transfer has finished. */
	while (!(UART0 -> UART_SR & UART_SR_ENDRX));
	/* Disable the PDC receiver. */
	UART0 -> UART_PTCR = UART_PTCR_RXTDIS;
	/* Enable the PDC receive complete interrupt. */
	UART0 -> UART_IER = UART_IER_ENDRX;
}

void fmr_push(struct _fmr_push_pull_packet *packet) {
	void *push_buffer = malloc(packet -> length);
	if (!push_buffer) {
		error_raise(E_MALLOC, NULL);
		return;
	}
	uart0_pull_wait(push_buffer, packet -> length);
	if (packet -> header.class != fmr_ram_load_class) {
		*(uintptr_t *)(packet -> call.parameters) = (uintptr_t)push_buffer;
		fmr_execute(packet -> call.index, packet -> call.function, packet -> call.argc, packet -> call.types, (void *)(packet -> call.parameters));
		free(push_buffer);
	} else {
		/* Add 1 for proper jump to thumb mode. */
		application_entry = push_buffer + 1;
	}
}

void fmr_pull(struct _fmr_push_pull_packet *packet) {
	void *pull_buffer = malloc(packet -> length);
	if (!pull_buffer) {
		error_raise(E_MALLOC, NULL);
		return;
	}
	*(uintptr_t *)(packet -> call.parameters) = (uintptr_t)pull_buffer;
	fmr_execute(packet -> call.index, packet -> call.function, packet -> call.argc, packet -> call.types, (void *)(packet -> call.parameters));
	uart0_push(pull_buffer, packet -> length);
	free(pull_buffer);
}

struct _fmr_packet packet;

void system_task(void) {
	/* Configure the USART peripheral. */
	usart_configure();
	/* Configure the UART peripheral. */
	uart0_configure();
	/* Configure the GPIO peripheral. */
	gpio_configure();
	/* Configure the SPI peripheral. */
	spi_configure();

	/* Pull an FMR packet asynchronously. */
	uart0_pull(&packet, sizeof(struct _fmr_packet));
	/* Enable the PDC receive complete interrupt. */
	UART0 -> UART_IER = UART_IER_ENDRX;

	/* -------- USER TASK -------- */

	char reset_msg[] = "Reset.\n";
	usart_push(reset_msg, sizeof(reset_msg));

repeat:
	// gpio_enable(PIO_PA0, 0);
	// PIOA -> PIO_OWER = PIO_PA0;

	/* Weird while(1) if behavior fix? */
	while (!application_entry) __NOP();

	char load_msg[] = "Loaded app. Launching.\n";
	usart_push(load_msg, sizeof(load_msg));

	((void (*)(void))(application_entry))();

	char done_msg[] = "Finished executing app.\n";
	usart_push(done_msg, sizeof(done_msg));

	application_entry = NULL;

	goto repeat;

}

void uart0_isr(void) {
	/* If an entire packet has been received, process it. */
	if (UART0 -> UART_SR & UART_SR_ENDRX) {
		/* Disable the PDC receiver. */
		UART0 -> UART_PTCR = UART_PTCR_RXTDIS;
		/* Clear the PDC RX interrupt flag. */
		UART0 -> UART_RCR = 1;
		/* Create a result. */
		struct _fmr_result result = { 0 };
		/* Process the packet. */
		fmr_perform(&packet, &result);
		/* Give the result back. */
		uart0_push(&result, sizeof(struct _fmr_result));
		/* Flush any remaining data that has been buffered. */
		while (UART0 -> UART_SR & UART_SR_RXRDY) UART0 -> UART_RHR;
		/* Pull the next packet asynchronously. */
		uart0_pull(&packet, sizeof(struct _fmr_packet));
	}
}

void system_init(void) {

	/* Disable the watchdog timer. */
	WDT -> WDT_MR = WDT_MR_WDDIS;

	/* Configure the EFC for 5 wait states. */
	EFC -> EEFC_FMR = EEFC_FMR_FWS(5);

	/* Configure the primary clock source. */
	if (!(PMC -> CKGR_MOR & CKGR_MOR_MOSCSEL)) {
		PMC -> CKGR_MOR = CKGR_MOR_KEY(0x37) | BOARD_OSCOUNT | CKGR_MOR_MOSCRCEN | CKGR_MOR_MOSCXTEN;
		for (uint32_t timeout = 0; !(PMC -> PMC_SR & PMC_SR_MOSCXTS) && (timeout ++ < CLOCK_TIMEOUT););
	}

	/* Select external 20MHz oscillator. */
	PMC -> CKGR_MOR = CKGR_MOR_KEY(0x37) | BOARD_OSCOUNT | CKGR_MOR_MOSCRCEN | CKGR_MOR_MOSCXTEN | CKGR_MOR_MOSCSEL;
	for (uint32_t timeout = 0; !(PMC -> PMC_SR & PMC_SR_MOSCSELS) && (timeout ++ < CLOCK_TIMEOUT););
	PMC -> PMC_MCKR = (PMC -> PMC_MCKR & ~(uint32_t)PMC_MCKR_CSS_Msk) | PMC_MCKR_CSS_MAIN_CLK;
	for (uint32_t timeout = 0; !(PMC -> PMC_SR & PMC_SR_MCKRDY) && (timeout++ < CLOCK_TIMEOUT););

	/* Configure PLLB as the master clock PLL. */
	PMC -> CKGR_PLLBR = BOARD_PLLBR;
	for (uint32_t timeout = 0; !(PMC -> PMC_SR & PMC_SR_LOCKB) && (timeout++ < CLOCK_TIMEOUT););

	/* Switch to the main clock. */
	PMC -> PMC_MCKR = (BOARD_MCKR & ~PMC_MCKR_CSS_Msk) | PMC_MCKR_CSS_MAIN_CLK;
	for (uint32_t timeout = 0; !(PMC -> PMC_SR & PMC_SR_MCKRDY) && (timeout++ < CLOCK_TIMEOUT););
	PMC -> PMC_MCKR = BOARD_MCKR;
	for (uint32_t timeout = 0; !(PMC -> PMC_SR & PMC_SR_MCKRDY) && (timeout++ < CLOCK_TIMEOUT););

	/* Allow the reset pin to reset the device. */
	RSTC -> RSTC_MR = RSTC_MR_KEY(0xA5) | RSTC_MR_URSTEN;
}

void system_deinit(void) {

}
