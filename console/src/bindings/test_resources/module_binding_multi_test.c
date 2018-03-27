// Compile with `gcc -g dwarf_parse_test.c -o dwarf_parse_test`

#include <stdint.h>

#define LM_ONE __attribute__((section(".lm.one")))
#define LM_TWO __attribute__((section(".lm.two")))

LM_ONE void test_one() {
    return;
}

void test_two(int count) {
    return;
}

LM_TWO int test_three(char letter) {
    return 10;
}

LM_ONE char *test_four(uint8_t first, uint16_t second, uint32_t third) {
    return "Hello";
}

int main() {
    return 0;
}
