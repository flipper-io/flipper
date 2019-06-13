#include "libflipper.h"
#include <twi.h>

LF_FUNC("i2c") int i2c_configure(void) {
    return lf_success;
}

/* Starts a read session. */
LF_FUNC("i2c") void i2c_start_read(uint8_t address, uint8_t length) {
}

/* Contiunues reading once a read session has begun. */
LF_FUNC("i2c") uint8_t i2c_read(void) {
    return 0;
}

/* Starts a write session. */
LF_FUNC("i2c") void i2c_start_write(uint8_t address, uint8_t length) {
}

/* Continues writing once a write session has begun. */
LF_FUNC("i2c") void i2c_write(uint8_t byte) {
}

/* Stops the active session. */
LF_FUNC("i2c") void i2c_stop(void) {
}
