Errors
===

`libflipper` incorperates its own error handling, and steers away from the error handling paradigm popularized by `errno`.

#### Throwing an error.

An error can be thrown using the following syntax.
```c
lf_error_raise(E_NUMBER, error_message("Something really bad happened as a result of %s", something_bad));
```
Where `E_NUMBER` is the error code defined in `error.h`.
