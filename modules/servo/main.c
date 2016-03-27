#include <stdio.h>

#include <string.h>

#include <flipper/flipper/flipper.h>

#include <servo.h>

/* ~ Simple program that uses the Flipper module 'servo.' ~ */

int main(int argc, char *argv[]) {
	
	if (argc < 3) {
		
		printf("\nUsage: servo [pin] [position]\n\n");
		
		return 0;
		
	}
	
	/* ~ Attach to a Flipper device over USB. ~ */
	
	flipper.attach(FLIPPER_SOURCE_NETWORK, "129.21.82.243");
	
	/* ~ Configure the 'servo' module. ~ */
	
	servo.configure();
	
	/* ~ Attach the servo to the approprite pin. ~ */
	
	servo.attach(atoi(argv[1]));
	
	/* ~ Rotate the servo to the desired position. ~ */
	
	servo.rotate(atoi(argv[2]));
	
	return 0;
	
}
