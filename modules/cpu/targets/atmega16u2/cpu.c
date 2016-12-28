#define __private_include__
#include <flipper/cpu.h>
#include <flipper/uart0.h>
#include <platforms/atmega16u2.h>

int cpu_configure(void) {
	/* Turn the SAM4S on. */
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Configure the SAM4S's power pin as an output. */
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_DDR);
	/* Deassert the SAM4S's test pin. */
	clear_bit_in_port(SAM_TEST_PIN, SAM_TEST_PORT);
	/* Configure the SAM4S's test pin as an output. */
	set_bit_in_port(SAM_TEST_PIN, SAM_TEST_DDR);
	/* Dessert the SAM4S's reset pin. */
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
	/* Configure the SAM4S's reset pin as an output. */
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_DDR);
	/* Deassert the erase pin. */
	clear_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_PORT);
	/* Configure the SAM4S's erase pin as an output. */
	set_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_DDR);
	return lf_success;
}

void cpu_reset(void) {
	/* Assert the SAM4S's reset pin. */
	clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
	/* Wait for 1 ms to simulate the press of a physical reset button, ensuring a thorough reset of the processor. */
	delay_ms(1);
	/* Release the simulated reset button. */
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
	/* Reset the USART bus. */
	uart0_configure();
}

void cpu_cycle(void) {
	/* Turn off the CPU. */
	cpu_power(false);
	/* Waut for a few milliseconds. */
	delay_ms(10);
	/* Turn on the CPU. */
	cpu_power(true);
}

void cpu_halt(void) {
	/* Assert the SAM4S's reset pin. */
	clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
}

void cpu_power(uint8_t power) {
	if (power) {
		set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
		set_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
	} else {
		clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
		clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	}
}

lf_error_t cpu_dfu(void) {

	/* 115.2k baud for DFU communication. */
	UBRR1L = 0x08;

	/* Assert the reset pin. */
	clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
	// /* Power down the SAM4S. */
	// clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	// /* Wait for power down. */
	// delay_ms(100);
	// /* Power the SAM4S back on. */
	// set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Put the SAM4S into erase mode by pulling its erase pin high. */
	set_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_PORT);
	/* Wait for the GPNVM bits to be erased. */
	delay_ms(500);
	delay_ms(500);
	delay_ms(500);
	delay_ms(500);
	delay_ms(500);
	delay_ms(500);
	delay_ms(500);
	delay_ms(500);
	delay_ms(500);
	/* Take the SAM4S out of erase mode by pulling its erase pin back low. */
	clear_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_PORT);
	// /* Power down the SAM4S. */
	// clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	// /* Wait for power down. */
	// delay_ms(100);
	// /* Power the SAM4S back on. */
	// set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Deassert the reset pin. */
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);

	/* Wait for the chip to boot. */
	delay_ms(500);

	return lf_success;

}
