#include <flipper.h>
#include <stdarg.h>

#define LF_ERROR_MESSAGE_STRINGS "no error", \
                                 "malloc failure", \
                                 "null pointer", \
                                 "overflow", \
                                 "invalid device", \
                                 "device not yet attached", \
                                 "device already attached", \
                                 "file already exists", \
                                 "file does not exist", \
                                 "message runtime packet overflow", \
                                 "message runtime error", \
                                 "endpoint error", \
                                 "libusb error", \
                                 "communication error", \
                                 "socket error", \
                                 "invalid module found", \
                                 "address resoultion failure", \
                                 "invalid error string", \
                                 "checksums do not match", \
                                 "invalid name", \
                                 "configuration error", \
                                 "acknowledgement error", \
                                 "type error", \
                                 "boundary error", \
                                 "timer error", \
                                 "timeout error", \
                                 "no task for pid", \
                                 "invalid task specified", \
                                 "packet subclass error", \
                                 "unimplemented error", \
                                 "test failed", \
                                 "uart0 write timeout", \
                                 "uart0 read timeout", \
                                 "usb error"

static const char *const err_strs[] = { LF_ERROR_MESSAGE_STRINGS };

static lf_err_t _lf_err;

LF_WEAK void _lf_assert(lf_err_t err, const char *func, int line, const char *fmt, ...) {
    lf_assert(err < LF_MAX_ERR, E_BOUNDARY, "err out of bounds");
    _lf_err = err;

    va_list args;
    va_start(args, fmt);

    printf(KRED "flipper runtime error" KNRM ": " KBLU "%s" KNRM " (0x%02x)\n" KGRN "  " "%s" ":%i: " KYEL, err_strs[err], err, func, line);
    vprintf(fmt, args);
    printf(KNRM "\n\n");

    va_end(args);

fail:
	return;
}

void lf_error_set(lf_err_t err) {

    lf_assert(err < LF_MAX_ERR, E_BOUNDARY, "err out of bounds");

    _lf_err = err;

fail:
    return;
}

lf_err_t lf_error_get(void) {
    return _lf_err;
}
