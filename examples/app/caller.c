#include <app.h>

int main(int argc, char *argv[]) {

	flipper_attach();
	app_configure();
	my_func(0x43);

	return 0;
}
