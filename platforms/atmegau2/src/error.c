#include "libflipper.h"
#include <atmegau2.h>

#define LF_ERROR(Err, Str) const char Err##_string[] PROGMEM = Str;
#include "errors.def"

const char *const err_strs[] PROGMEM = {
#define LF_ERROR(Err, Str) Err##_string,
#include "errors.def"
};

const char *lf_error_string(lf_err_t error) {
    return pgm_read_ptr(&err_strs[error]);
}

void _lf_assert(lf_err_t err, const char *func, int line, const char *fmt, ...) {
    _lf_err = err;

    va_list args;
    va_start(args, fmt);

    printf_P(PSTR(KRED "flipper runtime error" KNRM ": " KBLU "%S" KNRM " (0x%02x)\n" KGRN "  %s:%i: " KYEL),
             lf_error_string(err), err, func, line);
    vfprintf_P(stdout, fmt, args);
    printf_P(PSTR(KNRM "\n\n"));

    va_end(args);
}
