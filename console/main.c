#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include <fs/crc.h>

#include <flash/flash.h>

int main(int argc, char *argv[]) {
	
	flipper.attach(FLIPPER_SOURCE_FVM);
	
	char *data = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas nec dictum mi. Curabitur sem ante, sagittis sit amet blandit ac, varius sollicitudin lacus. Suspendisse diam sem, tristique ac neque non, finibus tempor diam. Donec accumsan ipsum eget finibus dignissim. Duis ut feugiat lectus. Etiam sit amet ligula metus. Donec eros orci, dapibus a lorem at, vestibulum maximus neque. Donec id tellus sit amet nisi aliquam luctus. Suspendisse erat augue, ultrices non tellus sed, tincidunt semper nunc. Praesent turpis metus, pharetra sit amet metus sed, facilisis lacinia libero.";
	
	device.invoke(0, 0, 100);
	
	usart.push(data, strlen(data));
	
	uint32_t ip = wifi.ip();
	
	printf("Got: 0x%08X\n\n", ip);
	
	return 0;
	
}