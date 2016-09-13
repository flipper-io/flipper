#define __private_include__
#include <private/sam-ba.h>
#include <platform/atmega16u2.h>

void sam_configure(void) {
    /* Turn the primary CPU on. */
    set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Configure the primary CPU's power pin as an output. */
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_DDR);
	/* Configure the primary CPU's test pin as an output. */
	set_bit_in_port(SAM_TEST_PIN, SAM_TEST_DDR);
	/* Dessert the primary CPU's reset pin. */
	clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
	/* Configure the primary CPU's reset pin as an output. */
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_DDR);
	/* Configure the primary CPU's erase pin as an output. */
	set_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_DDR);
}

void sam_suspend(void) {
	/* Assert the primary CPU's reset pin. */
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
}

void sam_engage(void) {
	/* Deassert the primary CPU's reset pin. */
	clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
}

void sam_power(uint8_t power) {
    if (power) {
        set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
    } else {
        clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
    }
}

void sam_reset(void) {
    /* Assert the primary CPU's reset pin. */
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
	/* Wait for 50 ms to simulate the press of a physical reset button, ensuring a thorough reset of the processor. */
	delay_ms(50);
	/* Release the simulated reset button. */
	clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
}

void sam_erase(void) {
	/* Disable interrupts. */
	cli();
	/* Indicate that we are busy. */
	led_set_rgb(LED_COLOR_BUSY);
	/* Power down the primary CPU. */
	sam_power(false);
	/* Wait for the primary CPU to power down. */
	delay_ms(500);
	/* Put the primary CPU into erase mode by pulling its erase pin high. */
	set_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_PORT);
	/* Power the primary CPU back on. */
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Wait for flash to be completely erased. */
	delay_ms(500);
	/* Power down the primary CPU. */
	clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	/* Take the primary CPU out of erase mode by pulling its erase pin back low. */
	clear_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_PORT);
	/* Wait for everything to settle. */
	delay_ms(500);
	/* Power the primary CPU back on. */
	sam_power(true);
	/* Indicate that the operation was successful. */
	led_set_rgb(LED_COLOR_SUCCESS);
	/* Re-enable interrupts. */
	sei();
}
