button
----

> The `button` module handles all user interaction with the tactile push-button that
> can be found in the upper rightmost corner of your device.

The module declaration can be found below.

```c
extern const struct _button {

	/* Configures the button hardware. */
	int (* configure)(void);

	/* Reads back the button state; returns 0 when released and 1 when pressed. */
	uint8_t (* read)(void);

} button;
```

### configure

The configure function
