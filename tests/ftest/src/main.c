#define __private_include__
#include <flipper.h>
#include <platform/posix.h>
#include <platform/fvm.h>

int main(int argc, char *argv[]) {

    error.pause();
    flipper.attach();

    printf(KGRN "Successfully attached to Flipper device.\n");

    return EXIT_SUCCESS;
}
