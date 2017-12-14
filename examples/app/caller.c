#include "main.h"

int main(int argc, char *argv[]) {

	flipper_attach();
	lf_set_debug_level(LF_DEBUG_LEVEL_ALL);
	my_func(0x10);

	return 0;
}
