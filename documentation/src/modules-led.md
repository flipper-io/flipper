led
===

> The `led` module handles all user interaction with the RGB LED that
> can be found at the top of your device.

This module contains the following functions:
- [configure](#configure)
- [rgb](#rgb)

-----------

## configure

The `configure` function initializes the LED hardware. This function is called
automatically on startup, and should not need to be called from
user applications.

```c
led.configure()
```
> **returns**: `int` representing the success of the operation.
> - `lf_success` when configuration succeeded.
> - `lf_error` when configuration failed.

**console syntax**
```
$ flipper led configure
```

-----------

## rgb

The `rgb` function sets the LED color.

```c
led.rgb(uint8_t r, uint8_t g, uint8_t b)
```
> **returns**: nothing

> **parameters**:
> - `uint8_t` **r** representing the red brightness value to be written to the LED.
> - `uint8_t` **g** representing the green brightness value to be written to the LED.
> - `uint8_t` **b** representing the blue brightness value to be written to the LED.
>
> ⚠️ **Note:** All parameters to `rgb` must be within the range **0** to **255**.

**console syntax**
```
$ flipper led rgb [red] [green] [blue]
```
