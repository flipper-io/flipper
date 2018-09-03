#include "libflipper.h"
#include "atmegau2.h"

#define LF_ERROR(Err, Str) const char Err##_string[] PROGMEM = Str;
#include "errors.def"

const char *const err_strs[] PROGMEM = {
#define LF_ERROR(Err, Str) Err##_string,
#include "errors.def"
};

void _lf_assert(lf_err_t err, const char *func, int line, const char *fmt, ...) {
    _lf_err = err;

    va_list args;
    va_start(args, fmt);

    const char *error_str = pgm_read_ptr(&err_strs[err]);
    printf_P(PSTR(KRED "flipper runtime error" KNRM ": " KBLU "%S" KNRM " (0x%02x)\n" KGRN "  " "%s" ":%i: " KYEL), error_str, err, func, line);
    vfprintf_P(stdout, fmt, args);
    printf_P(PSTR(KNRM "\n\n"));

    va_end(args);
}
