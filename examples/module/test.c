#include <flipper.h>
#include <module/module.h>

int main(int argc, char *argv[]) {

    flipper.attach();
    module.configure();
    module.test();

    return EXIT_SUCCESS;
}
