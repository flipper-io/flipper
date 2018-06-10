#include <flipper.h>
#include <stdarg.h>

#include <flipper/error.h>

static lf_err_t _lf_err;
static const char *err_strs[] = { LF_ERROR_MESSAGE_STRINGS };

void _lf_assert(lf_err_t err, const char *func, const char *fmt, ...) {
    lf_assert(err < LF_MAX_ERR, E_BOUNDARY, "err out of bounds");
    _lf_err = err;

    va_list args;
    va_start(args, fmt);
    printf(KRED "flippper runtime error" KNRM ": " KBLU "%s" KNRM " (0x%02x)\n" KGRN "  %s: " KYEL, err_strs[err], err, func);
    vprintf(fmt, args);
    printf(KNRM "\n");
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
