#define __private_include__

#include <flipper/flipper.h>

#include <master/flipper.h>
#include <math.h>
#include <network/network.h>
#include <unistd.h>
#include <fmr/fmr.h>

static inline int to_color(float c) { return fabsf(sinf(c) * 50.0f); }

int main(int argc, char *argv[]) {
	
	printf("Welcome to the Flipper Console!\n\n");
	
	flipper.attach("elroy", FLIPPER_SOURCE_NETWORK, "129.21.81.81");
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
	
	return EXIT_SUCCESS;
	
}