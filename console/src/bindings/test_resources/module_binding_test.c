// Compile with `gcc -g dwarf_parse_test.c -o dwarf_parse_test`

#include <stdint.h>

#define LF_FUNC __attribute__((section(".lf.funcs")))

LF_FUNC void test_one() {
    return;
}

void test_two(int count) {
    return;
}

LF_FUNC int test_three(char letter) {
    return 10;
}

LF_FUNC char *test_four(uint8_t first, uint16_t second, uint32_t third) {
    return "Hello";
}

int main() {
    return 0;
}
