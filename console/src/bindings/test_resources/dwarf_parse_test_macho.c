// Compiled on MacOS with "gcc -g dwarf_parse_test_macho.c -o dwarf_parse_test_macho"

#include <flipper/flipper.h>

__attribute__((section("__TEXT,.lf.funcs"))) int test(int a, char b, long c) {
    return 0;
}