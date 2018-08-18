#include <flipper.h>

int main(int argc, char *argv[]) {

	carbon_attach_hostname("localhost");

    led_rgb(0, 0, 0);

	return EXIT_SUCCESS;
}
