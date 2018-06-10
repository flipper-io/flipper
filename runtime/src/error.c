#include <flipper.h>
#include <stdarg.h>

#ifdef __AVR__
#define STROP "%S"
#define STRL(name, str) const char name[] PROGMEM = str;
#else
#define STROP "%s"
#define STRL(name, str) const char name[] = str;
#endif

#define MSG KRED "flippper runtime error" KNRM ": " KBLU STROP KNRM " (0x%02x)\n" KGRN "  " "%s" ": " KYEL

STRL(E_OK_STR, "no error");
STRL(E_MALLOC_STR, "malloc failure");
STRL(E_NULL_STR, "null pointer");
STRL(E_OVERFLOW_STR, "overflow");
STRL(E_NO_DEVICE_STR, "invalid device");
STRL(E_NOT_ATTACHED_STR, "device not yet attached");
STRL(E_ALREADY_ATTACHED_STR, "device already attached");
STRL(E_FS_EXISTS_STR, "file already exists");
STRL(E_FS_NO_FILE_STR, "file does not exist");
STRL(E_FMR_OVERFLOW_STR, "message runtime packet overflow")
STRL(E_FMR_STR, "message runtime error");
STRL(E_ENDPOINT_STR, "endpoint error");
STRL(E_LIBUSB_STR, "libusb error");
STRL(E_COMMUNICATION_STR, "communication error");
STRL(E_SOCKET_STR, "socket error");
STRL(E_MODULE_STR, "invalid module found");
STRL(E_RESOULTION_STR, "address resoultion failure");
STRL(E_STRING_STR, "invalid error string");
STRL(E_CHECKSUM_STR, "checksums do not match");
STRL(E_NAME_STR, "invalid name");
STRL(E_CONFIGURATION_STR, "configuration error");
STRL(E_ACK_STR, "acknowledgement error");
STRL(E_TYPE_STR, "type error");
STRL(E_BOUNDARY_STR, "boundary error");
STRL(E_TIMER_STR, "timer error");
STRL(E_TIMEOUT_STR, "timeout error");
STRL(E_NO_PID_STR, "no task for pid");
STRL(E_INVALID_TASK_STR, "invalid task specified");
STRL(E_SUBCLASS_STR, "packet subclass error");
STRL(E_UNIMPLEMENTED_STR, "unimplemented error");
STRL(E_TEST_STR, "test failed");
STRL(E_USB_STR, "uart0 write timeout");
STRL(E_UART0_WRITE_TIMEOUT_STR, "uart0 read timeout");
STRL(E_UART0_READ_TIMEOUT_STR, "usb error")

#define LF_ERROR_MESSAGE_STRINGS E_OK_STR, \
                                 E_MALLOC_STR, \
                                 E_NULL_STR, \
                                 E_OVERFLOW_STR, \
                                 E_NO_DEVICE_STR, \
                                 E_NOT_ATTACHED_STR, \
                                 E_ALREADY_ATTACHED_STR, \
                                 E_FS_EXISTS_STR, \
                                 E_FS_NO_FILE_STR, \
                                 E_FMR_OVERFLOW_STR, \
                                 E_FMR_STR, \
                                 E_ENDPOINT_STR, \
                                 E_LIBUSB_STR, \
                                 E_COMMUNICATION_STR, \
                                 E_SOCKET_STR, \
                                 E_MODULE_STR, \
                                 E_RESOULTION_STR, \
                                 E_STRING_STR, \
                                 E_CHECKSUM_STR, \
                                 E_NAME_STR, \
                                 E_CONFIGURATION_STR, \
                                 E_ACK_STR, \
                                 E_TYPE_STR, \
                                 E_BOUNDARY_STR, \
                                 E_TIMER_STR, \
                                 E_TIMEOUT_STR, \
                                 E_NO_PID_STR, \
                                 E_INVALID_TASK_STR, \
                                 E_SUBCLASS_STR, \
                                 E_UNIMPLEMENTED_STR, \
                                 E_TEST_STR, \
                                 E_USB_STR, \
                                 E_UART0_WRITE_TIMEOUT_STR, \
                                 E_UART0_READ_TIMEOUT_STR, \

static const char *const err_strs[] = { LF_ERROR_MESSAGE_STRINGS };

static lf_err_t _lf_err;

void _lf_assert(lf_err_t err, const char *func, const char *fmt, ...) {
    lf_assert(err < LF_MAX_ERR, E_BOUNDARY, "err out of bounds");
    _lf_err = err;

    va_list args;
    va_start(args, fmt);

#ifdef __AVR__
    printf_P(PSTR(MSG), err_strs[err], err, func);
    vfprintf_P(stdout, fmt, args);
    printf_P(PSTR(KNRM "\n\n"));
#else
    printf(MSG, err_strs[err], err, func);
    vprintf(fmt, args);
    printf(KNRM "\n\n");
#endif

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
