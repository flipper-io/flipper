// Compile with `gcc -g elf_section_test.c -o elf_section_test`

#include <stdio.h>

#define LF_FUNC __attribute__((section(".lf.funcs")))

// This function should appear in the ".lf.funcs" binary section.
LF_FUNC int sayHi(char *name) {
	printf("Hi, %s\n");
}

int main(int argc, char *argv[]) {
	printf("Hello!\n");
	sayHi("George");
	return 0;
}
