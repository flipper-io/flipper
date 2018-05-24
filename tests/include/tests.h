#ifndef __tests__
#define __tests__

#define lf_expect_error() lf_assert(lf_error_get() != E_OK, fail, E_UNIMPLEMENTED, "Expected error not thrown on line %d in %s.", __LINE__, __FILE__); lf_error_clear();
#define lf_expect_success() lf_assert(lf_error_get() == E_OK, fail, E_UNIMPLEMENTED, "Unexpected error thrown on line %d in %s.", __LINE__, __FILE__); lf_error_clear();

#endif
