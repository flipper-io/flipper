# Drivers
-

All of Flipper's drivers are simple data structures that organize pointers to driver functions. A driver is defined as `const struct _driver_name { ... } driver;` where `...` contains the definition of at least one function pointer aside from `configure`.

All drivers contain a configuration function `void (* configure)(void)`. Whenever a given driver is loaded, this function is called.

## Button

As its name suggests, this driver is responsible for reading the current state of Flipper's button.

`.read()` reads the current state of the button.