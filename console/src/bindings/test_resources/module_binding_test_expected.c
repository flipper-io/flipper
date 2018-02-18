#include <flipper.h>

LF_MODULE(_user, "user", "User module description", NULL, NULL);

const struct _user_interface user {
    user_test_four,
    user_test_three,
    user_test_one
};

LF_WEAK char* user_test_four(uint8_t first, uint16_t second, uint32_t third) {
    char* result = lf_invoke(lf_get_current_device(), &_user, _user_test_four, lf_uint8_t, lf_args(lf_infer(first), lf_infer(second), lf_infer(third)));
}

LF_WEAK int user_test_three(char letter) {
    int result = lf_invoke(lf_get_current_device(), &_user, _user_test_three, lf_uint32_t, lf_args(lf_infer(letter)));
}

LF_WEAK void user_test_one() {
    void result = lf_invoke(lf_get_current_device(), &_user, _user_test_one, lf_void_t, lf_args());
}
