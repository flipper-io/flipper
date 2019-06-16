#include <flipper.h>
#include <qux.h>

LF_FUNC int qux_func(uint8_t a) {
    printf("Hello world!");
    gpio_enable(IO_1, 0);
    gpio_write(IO_1, 0);
    return 0;
}
