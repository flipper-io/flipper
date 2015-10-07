#define __private_include__

#include <sam/sam.h>

#include <platform/atmega.h>

#include <led/led.h>

/* ~ This function configures the main processor. ~ */

void sam_configure(void) {
	
	/* ~ Configure the 7S' power pin as an output. ~ */
	
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_DDR);
	
	/* ~ Turn the 7S on. ~ */
	
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	
	/* ~ Configure the 7S' test pin as an output. ~ */
	
	set_bit_in_port(SAM_TEST_PIN, SAM_TEST_DDR);
	
	/* ~ Configure the 7S' reset pin as an output. ~ */
	
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_DDR);
	
	/* ~ Configure the 7S' erase pin as an output. ~ */
	
	set_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_DDR);
	
}

/* ~ This function turns the power to the main processor on or off. ~ */

void sam_set_power(bool power) {

	if (power) set_bit_in_port(SAM_POWER_PIN, PORTD);
	
	else clear_bit_in_port(SAM_POWER_PIN, PORTD);
	
}

/* ~ This function issues a hardware reset to the main processor. ~ */

void sam_reset(void) {

	/* ~ Cycling the 7S' 'reset' line on the U2 will cause a transistor to pull the reset pin on the 7S to ground and generate a hardware reset. ~ */
	
	set_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);
	
	/* ~ Wait for 50 ms to simulate the press of a physical reset button, ensuring a thorough reset of the processor. ~ */
	
	delay_ms(50);
	
	/* ~ Release the simulated reset button. ~ */
	
	clear_bit_in_port(SAM_RESET_PIN, SAM_RESET_PORT);

}

/* ~ This function puts the main processor into DFU mode. ~ */

void sam_load_dfu(void) {
	
	/* ~ Disable interrupts. ~ */
	
	disable_interrupts();
	
	/* ~ Indicate that we are busy. ~ */
	
	led_set_rgb(LED_COLOR_BUSY);
	
	/* ~ Turn the SAM7S off. ~ */
	
	clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	
	/* ~ Wait for the 7S to completely power down. ~ */
	
	delay_seconds(1);
	
	/* ~ Set the 7S' test pin high. ~ */
	
	set_bit_in_port(SAM_TEST_PIN, SAM_POWER_PORT);
	
	/* ~ Power the 7S on. ~ */
	
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	
	/* ~ Atmel recommends waiting 10 seconds for the SAM-BA to be copied into the 7S' flash memory space. ~ */
	
	delay_seconds(5);
	
	delay_seconds(5);
	
	/* ~ Power down the 7S. ~ */
	
	clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	
	/* ~ Release the 7S' TST pin. ~ */
	
	clear_bit_in_port(SAM_TEST_PIN, SAM_POWER_PORT);
	
	/* ~ Again, wait for the 7S to completely power down. ~ */
	
	delay_seconds(1);
	
	/* ~ Power the 7S back on. ~ */
	
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	
	/* ~ Wait for the processor to load the SAM-BA. ~ */
	
	delay_seconds(1);
	
	/* ~ Indicate that the operation was successful. ~ */
	
	led_set_rgb(LED_COLOR_SUCCESS);
	
	/* ~ Re-enable interrupts. ~ */
	
	enable_interrupts();
	
}

/* ~ This function completely erases the main processor's flash memory. ~ */

void sam_format(void) {
	
	/* ~ Disable interrupts. ~ */
	
	disable_interrupts();
	
	/* ~ Indicate that we are busy. ~ */
	
	led_set_rgb(LED_COLOR_BUSY);
	
	/* ~ Power down the 7S. ~ */
	
	clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	
	/* ~ Put the 7S into erase mode by pulling its erase pin high. ~ */
	
	set_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_PORT);
	
	/* ~ Wait for the 7S to power down. ~ */
	
	delay_seconds(1);
	
	/* ~ Power the 7S back on. ~ */
	
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	
	/* ~ Wait for flash to be completely erased. ~ */
	
	delay_seconds(5);
	
	delay_seconds(5);
	
	/* ~ Power down the 7S. ~ */
	
	clear_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	
	/* ~ Wait for everything to settle. ~ */
	
	delay_seconds(1);
	
	/* ~ Take the 7S out of erase mode by pulling its erase pin back low. ~ */
	
	clear_bit_in_port(SAM_ERASE_PIN, SAM_ERASE_PORT);
	
	/* ~ Power the 7S back on. ~ */
	
	set_bit_in_port(SAM_POWER_PIN, SAM_POWER_PORT);
	
	/* ~ Indicate that the operation was successful. ~ */
	
	led_set_rgb(LED_COLOR_SUCCESS);
	
	/* ~ Re-enable interrupts. ~ */
	
	enable_interrupts();
	
}