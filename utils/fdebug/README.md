# fdebug

`fdebug` is a program that redirects the hardware's `stdout` file to the `stdout` of the host.

When `fdebug` is running, any call to `printf` on the device will appear on the host. This provides behavior similar to semihosting, but doesn't require a debugger to be running.

`fdebug` takes no command line arguments. To begin piping debug messages from the device to `stdout`, simply start the program:

```
fdebug
```