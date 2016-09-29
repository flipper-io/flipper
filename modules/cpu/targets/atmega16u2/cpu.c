#define __private_include__
#include <flipper/cpu.h>
#include <platform/atmega16u2.h>

void cpu_configure(void) {
    /* Turn the SAM4S on. */
    set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Configure the SAM4S's power pin as an output. */
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_DDR);
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
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
	/* Wait for 50 ms to simulate the press of a physical reset button, ensuring a thorough reset of the processor. */
	delay_ms(50);
	/* Release the simulated reset button. */
	clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
}

void cpu_hault(void) {
	/* Assert the SAM4S's reset pin. */
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
}

void cpu_power(uint8_t power) {
    if (power) {
        set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
    } else {
        clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
    }
}

void cpu_dfu(void) {
	/* Disable interrupts. */
	cli();
	/* Power down the SAM4S. */
    clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Put the SAM4S into erase mode by pulling its erase pin high. */
	set_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_PORT);
	/* Power the SAM4S back on. */
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Wait for flash to be completely erased. */
	delay_ms(500);
	/* Power down the SAM4S. */
	clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Take the SAM4S out of erase mode by pulling its erase pin back low. */
	clear_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_PORT);
	/* Power the SAM4S back on. */
    set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Re-enable interrupts. */
	sei();
}
