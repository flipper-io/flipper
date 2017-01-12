FMR
===

The Flipper Message Runtime, or **FMR** for short, is the component of `libflipper` that makes all of the magic happen. The message runtime is responsible for transferring information about how to invoke functions, transfer data, load modules, launch applications, and more to the device attached to the current instance of [libflipper](./libflipper.html).

---

### Performing an invocation.

The most basic functionality enabled by the message runtime is the ability to invoke functions remotely. As such, the message runtime serves as the backbone for the remote procedure calling functionality of the toolbox.

To invoke a function, an [lf_module](./libflipper-modules.html) reference must have been obtained using the `lf_bind` function.

The routine used to perform the invocation is as below.
```c
lf_invoke(_my_module, _my_function, NULL);
```
This syntax instructs the message runtime to call the function `_my_function` located within the module `_my_module` that has already been loaded on the device to which the module belongs. The `NULL` passed to the parameters field of the `lf_invoke` function tells the message runtime that no arguments are to be passed to the target function.

---

### Sending data to the device.

---

### Receiving data from the device.
