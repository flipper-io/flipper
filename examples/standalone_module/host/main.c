#include <flipper.h>
#include <qux.h>

int main(int argc, char *argv[]) {

    carbon_attach();
    qux_func(0x10);

    return 0;
}
