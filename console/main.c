#define __private_include__

#include <flipper/flipper.h>

#include <master/flipper.h>
#include <math.h>
#include <network/network.h>
#include <unistd.h>
#include <fmr/fmr.h>

static inline int to_color(float c) { return fabsf(sinf(c) * 75.0f); }

int a = 0;

int main(int argc, char *argv[]) {
	
	printf("Welcome to the Flipper Console!\n\n");
	
	//flipper.attach(FLIPPER_SOURCE_NETWORK, "129.21.81.81");
	
	flipper.attach(FLIPPER_SOURCE_USB);
	
	while (1) {
	
		sleep(1);
		
		//device.invoke(_led, _led_set_rgb, 3, 0, 0, a * 25);
		
		uint32_t ret = device.invoke(_button, _button_read, 0);
		
		printf("RET: 0x%04X\n", ret);
		
		a ^= 1;
		
	}
	
#if 0
	
	float red = 1.0;
	float green = 2.0;
	float blue = 3.0;
	float increment = 0.4;
	
	while (1) {
		usleep(increment * 100000);
		red += increment;
		green += increment;
		blue += increment;
		int r = to_color(red);
		int g = to_color(green);
		int b = to_color(blue);
		device.invoke(_led, _led_set_rgb, 3, r, g, b);
		printf("Sent r: %d g: %d b: %d\n", r, g, b);
	}
 
#endif
	return EXIT_SUCCESS;
	
}