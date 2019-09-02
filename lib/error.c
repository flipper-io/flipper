#include "libflipper.h"
#include <stdarg.h>

lf_err_t _lf_err;

#define LF_ERROR(Err, Str) LF_WEAK const char Err##_string[] = Str;
#include "errors.def"

LF_WEAK const char *const err_strs[] = {
#define LF_ERROR(Err, Str) Err##_string,
#include "errors.def"
};

LF_WEAK const char *lf_error_string(lf_err_t error) {
    if (error >= (sizeof(err_strs)/sizeof(const char *const))) {
        return "error out of bounds";
    }
    return err_strs[error];
}

LF_WEAK void _lf_debug(const char *fmt, ...) {

    va_list args;
    va_start(args, fmt);

    vprintf(fmt, args);

    va_end(args);
}

LF_WEAK void _lf_assert(lf_err_t err, const char *func, int line, const char *fmt, ...) {
    _lf_err = err;

    va_list args;
    va_start(args, fmt);

    printf(KRED "flipper runtime error" KNRM ": " KBLU "%s" KNRM " (0x%02x)\n" KGRN "  %s:%i: " KYEL, lf_error_string(err),
           err, func, line);
    vprintf(fmt, args);
    printf(KNRM "\n\n");

    va_end(args);
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
