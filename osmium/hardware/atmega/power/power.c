#define __private_include__

#include <platform/atmega.h>

#include <button/button.h>

#include <led/led.h>

#include <power.h>

#define DEBOUNCE 25

#define WAIT 50

/* ~ This function will put the U2 into a power saving mode and hang activity until the power button is held. ~ */

void flipper_sleep(void) {
	
	/* ~ Under this sleep mode, the U2 will be completely powered down. ~ */
	
	set_sleep_mode(SLEEP_MODE_PWR_DOWN);
	
	/* ~ Enable sleep mode. ~ */
	
	sleep_enable();
	
	/* ~ Enable interrupts because sleep_enable() diables them. ~ */
	
	enable_interrupts();
	
	/* ~ Put the device to sleep. ~ */
	
	sleep_mode();
	
}

void flipper_reset(void) {
	
	/* ~ Disable all interrupts. ~ */
	
	disable_interrupts();
	
	/* ~ Turn off the LED. ~ */
	
	led_set_rgb(LED_OFF);
	
	/* ~ The watchdog timer is used to generate a hardware reset on the U2. Configure it to time out in 15 ms. ~ */
	
	wdt_enable(WDTO_15MS);
	
	/* ~ Wait for imminent doom. ~ */
	
	while (true);
	
}

void u2_total_reset(void) {
	
	/* ~ When the power button is pressed, a reset is triggered. The 7S is reset first, followed by the U2. ~ */
	
	//sam_reset();
	
	delay_ms(WAIT);
	
	/* ~ Save the current state of the U2 in EEPROM indicating that we want to power on directly after reset instead of waiting for the power button to be pressed. ~ */
	
	u2_power_on_after_reset(true);
	
	/* ~ Trigger a reset. ~ */
	
	flipper_reset();
	
}

void flipper_power_off(void) {
	
	/* ~ Turn off the LED. ~ */
	
	led_set_rgb(LED_OFF);
	
	/* ~ Turn off the 7S. ~ */
	
	//sam.power(OFF);
	
	/* ~ Don't power on after reset. ~ */
	
	u2_power_on_after_reset(false);
	
	/* ~ Trigger a reset. Upon reset, the U2 will wait for the power button to be held before initializing. ~ */
	
	flipper_reset();
	
}

/* ~ When the power button is pressed, this function is called. ~ */

void button_pressed(void) {
	
	u2_total_reset();
	
}

/* ~ Keep track of whether or not we are on. May be better to read the WDT reset bit. ~ */

bool u2_power_on = false;

/* ~ When the power button is held, this function is called. ~ */

void button_held(int seconds) {
	
	/* ~ If the power is on, begin the power down sequence. ~ */
	
	if (u2_power_on)
		
		flipper_power_off();
	
	/* ~ Otherwise, power the device on by resetting it and initializing it normally. ~ */
	
	else
		
		u2_total_reset();
	
}

/* ~ Interrupt Service Routine for a press of the power button. ~ */

unsigned int button_held_time;

ISR (INT0_vect) {
	
	/* Wait for the button to debounce. */
	
	delay_ms(DEBOUNCE);
	
	/* Every interrupt, add the debounce period to the total time the button has been held. */
	
	button_held_time += DEBOUNCE;
	
	/* If the button has debounced and the signal remains high, check to see how much time has passed since the last interrupt and react accordingly. */
	
	if (button_read()) __vector_1();
 
	else {
		
		/* ~ If the button has been held for 1 or more seconds, then trigger a button hold. ~ */
		
		if (button_held_time >= 1000) {
			
			/* ~ Zero the total time the button has been held for the next button press. ~ */
			
			button_held_time = 0;
			
			button_held(button_held_time / 1000);
			
		}
		
		/* ~ Otherwise, if the button has been 'held' for less than 1 second, and the power is on, trigger a button press. ~ */
		
		else if (u2_power_on) {
			
			/* ~ Zero the total time the button has been held for the next button press. ~ */
			
			button_held_time = 0;
			
			button_pressed();
			
		}
		
		/* ~ Finally, if the U2 has not been powered on, then send it back into sleep mode. ~ */
		
		else {
			
			/* ~ Zero the total time the button has been held for the next button press. ~ */
			
			button_held_time = 0;
			
			flipper_sleep();
			
		}
		
	}
	
}