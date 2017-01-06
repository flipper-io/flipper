button
===

> The `button` module handles all user interaction with the tactile push-button that
> can be found in the upper rightmost corner of your device.

This module contains the following functions:
- [configure](#configure)
- [read](#read)

-----------

## configure

The `configure` function initializes the button hardware. This function is called
automatically on startup, and should not need to be called from
user applications.

```c
button.configure()
```
> **returns**: `int` representing the success of the operation.
> - `lf_success` when configuration succeeded.
> - `lf_error` when configuration failed.

**console syntax**
```
$ flipper button configure
```

-----------

## read

The `read` function returns the button state as described above.

```c
button.read()
```
> **returns**: `uint8_t` representing the button state.
> - `0` when the button is released.
> - `1` when the button is pressed.

**console syntax**
```
$ flipper button read
```
