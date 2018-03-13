#include "main.h"

void *qux_interface[] = {
    &qux_func
};

LF_MODULE(qux, "qux", qux_interface);

LF_FUNC("qux") int qux_func(uint8_t a) {
    printf("Hello world!");
    return 0;
}
