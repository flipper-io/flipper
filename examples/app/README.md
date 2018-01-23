# Package Example

This example demonstrates how to build a package. A package is a collection of functions that are installed on the device and can be remotely invoked from any device running libflipper.

### Build the Package
```
make
```

### Install the Package
```
make install
```

The executable code will now be on the attached Flipper device.

## Running

A package doesn't have an entry point or main function, so no code is run when it is loaded. Instead, code is run when a call is made to a function that exists inside the package.

This example provides a test program in the `host` folder. This program calls the `my_func` function and is built for the host. To execute the function `my_func` on the device, simply call the host program.

```
./build/host/example
```

If you have an LED attached to a pin on the board, it will change state each time the test program is run. The host program is remotely invoking the function that does this on the attached device.
