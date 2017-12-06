#include <app.h>

int main(int argc, char *argv[]) {

	flipper_attach();
	lf_set_debug_level(LF_DEBUG_LEVEL_ALL);
	app_configure();
	my_func(0x10);

	return 0;
}
