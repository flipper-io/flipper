#include <flipper.h>
#include <qux/qux.h>

int main(int argc, char *argv[]) {

    flipper.attach();
    qux.configure();
    qux.test();

    return EXIT_SUCCESS;
}
