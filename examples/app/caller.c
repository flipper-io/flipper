#include <app.h>

int main(int argc, char *argv[]) {

	carbon_attach_hostname("localhost");
    app_configure();
    my_func(0x43);

	return 0;
}
