#include <flipper/flipper.h>

LF_FUNC int swd_configure(void) {
    printf("Configured the swd.\n");
    return lf_success;
}
