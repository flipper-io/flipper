#include <flipper.h>

LF_MODULE(_user, "user", "User module description", NULL, NULL);

const struct _user user {
    user_test_four,
    user_test_three,
    user_test_one
};

LF_WEAK char* user_test_four(uint8_t first, uint16_t second, uint32_t third) {
    char* result = lf_invoke(&_user, _user_test_four, fmr_uint8_t, fmr_args(fmr_infer(first), fmr_infer(second), fmr_infer(third)));
}

LF_WEAK int user_test_three(char letter) {
    int result = lf_invoke(&_user, _user_test_three, fmr_uint32_t, fmr_args(fmr_infer(letter)));
}

LF_WEAK void user_test_one() {
    void result = lf_invoke(&_user, _user_test_one, fmr_void_t, fmr_args());
}
