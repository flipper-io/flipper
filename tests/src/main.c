/* Test suite for libflipper. */

#include <flipper.h>

extern int dyld_test(void);
extern int ll_test(void);

int main(int argc, char *argv[]) {

    lf_assert(dyld_test() == lf_success, failure, E_TEST, "Failed dyld_test.");
    lf_assert(ll_test() == lf_success, failure, E_TEST, "Failed ll_test.");

    return EXIT_SUCCESS;
failure:
    return EXIT_FAILURE;
}
