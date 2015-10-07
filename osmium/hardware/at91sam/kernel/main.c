#define __private_include__

#include <flipper/flipper.h>

int main(void) {
	
	io.configure();
	
	io.direction(8, OUTPUT);
	
	io.write(8, true);

}