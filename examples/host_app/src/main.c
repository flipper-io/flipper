#include <flipper.h>

int main(int argc, char *argv[]) {
    lf_attach();
    printf("Loaded my application!");
    gpio_enable(IO_1, 0);
    gpio_write(IO_1, 0);
    return 0;
}
