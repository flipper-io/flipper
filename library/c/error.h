/* error.h - Standardizes the notion of error handling across all attached devices. */

#ifndef __lf_error_h__
#define __lf_error_h__

/* Success code macros. */
#define lf_success 1
#define lf_error 0

/* Short hand for raising errors based on the truth of a condition. */

#ifdef LF_CONFIG_OMIT_ERRORS
#define lf_assert(cond, err, fmt, ...) if (!(cond)) { goto fail; }
#else
#define lf_assert(cond, err, fmt, ...) if (!(cond)) { _lf_assert(err, __func__, __LINE__, fmt, ##__VA_ARGS__); goto fail; }
#endif

/* Enumerate all of the error codes. */
typedef enum {
    #define LF_ERROR(Err, Str) Err,
    #include "errors.def"
} lf_err_t;

/* Raises an error internally to the current context of libflipper. */
extern void _lf_assert(lf_err_t err, const char *func, int line, const char *format, ...) __attribute__ ((format (printf, 4, 5)));

extern lf_err_t _lf_err;

void lf_error_set(lf_err_t err);
lf_err_t lf_error_get(void);
const char *lf_get_err_str(lf_err_t err);

#endif
