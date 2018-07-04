#include <flipper.h>

int main(int argc, char *argv[]) {
    printf("Loaded my application!");
    gpio_enable(IO_1, 0);
    gpio_write(IO_1, 0);
    return 0;
}
