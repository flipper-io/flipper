#include <flipper.h>

LF_MODULE(_Test, "Test", "", NULL, NULL);

const struct _Test_interface Test {
    Test_test
};

LF_WEAK int Test_test(int a, char b, long int c) {
    int result = lf_invoke(lf_get_selected(), &_Test, _Test_test, lf_uint32_t, lf_args(lf_infer(a), lf_infer(b), lf_infer(c)));
}
