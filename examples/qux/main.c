#define __private_include__
#include <flipper.h>
#include <qux/qux.h>
#include <flipper/platforms/fvm/fvm.h>

int main(int argc, char *argv[]) {

    flipper_attach_endpoint("fvm", &lf_fvm_ep);

    /* Configure the qux module. */
    qux.configure();

    /* Invoke the qux_a funtion in the qux module. */
    lf_invoke(&_qux, _qux_a, NULL);

    return 0;
}
