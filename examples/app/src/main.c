#include <flipper.h>

int main(int argc, char *argv[]) {
    gpio_enable(IO_1, 0);
    gpio_write(IO_1, 0);
    return 0;
}
