#define __private_include__
#include <flipper/cpu.h>
#include <flipper/uart.h>
#include <platform/atmega16u2.h>

void cpu_configure(void) {
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
}

void cpu_reset(void) {
	/* Assert the SAM4S's reset pin. */
	clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
	/* Wait for 50 ms to simulate the press of a physical reset button, ensuring a thorough reset of the processor. */
	delay_ms(500);
	/* Release the simulated reset button. */
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
}

void cpu_halt(void) {
	/* Assert the SAM4S's reset pin. */
	clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
}

void cpu_power(uint8_t power) {
	if (power) {
		uart_enable();
		set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
		set_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
	} else {
		clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
		clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
		uart_disable();
	}
}

void cpu_dfu(void) {
	/* Put the SAM4S into erase mode by pulling its erase pin high. */
	set_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_PORT);
	/* Assert the reset pin. */
	clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
	/* Power down the SAM4S. */
	clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Power the SAM4S back on. */
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Wait for the GPNVM bits to be erased. */
	delay_ms(500);
	/* Take the SAM4S out of erase mode by pulling its erase pin back low. */
	clear_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_PORT);
	/* Power down the SAM4S. */
	clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Power the SAM4S back on. */
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Deassert the reset pin. */
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
}
