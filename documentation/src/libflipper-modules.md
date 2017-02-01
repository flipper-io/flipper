Modules
===

By definition, a module lives on the device's hardware. Modules expose functions that control the peripherals attached to the device in a predefined way.

#### Use case example.

Say you wanted write a driver for a new I2C sensor. In the early stages of debugging, you would use the [I2C module](./modules-i2c.html) to interact with the sensor, say from a Python application, on your PC. Once the basic functionality of the driver has been implemented, it becomes practical to consolidate the prototyped functionality onto your device. This is where modules come in to play. You have a function called `my_sensor.read()` that encapsulates dozens calls to the I2C module. Although the [message runtime](./libflipper-fmr.html) is a powerful tool that can be used for protoyping an debugging, it is not as fast as code running natively on the device. By making your sensor driver into a module, you could reduce the number of message runtime calls needed to interact with your sensor from dozens to just the one needed to invoke your module's `read` function.

This documentation entry goes into detail about the structure of modules and how to write them.

---

### Module structure.

All `libflipper` modules have the same structure consisting of three major components.
- The virtual interface. (the module's jumptable)
- The overlay. (the module's `enum`)
- The lf_module.

---

#### Virtual Interface

The primary component of a module is the module's *virtual interface*, which is a jumptable that enumerates all of the function addresses exposed by that module. The virtual interface of a simple module called `my_module` that exposes a single function `test` is declared as follows.
```c
extern const struct _my_module {
    my_module_configure,
    my_module_test
} my_module;
```
You may have noticed that the module also must expose a configuration function. This function is executed when the module is first loaded. The module author may choose to leave this function empty, but due to expectations of the loader its address must be present as the first entry in the module's jumptable.

#### Overlay

Once the virtual interface has been declared, the message runtime must have a way to know the layout of the functions that exist within the module. This is accomplished using an `enum` to enumerate the indices of the functions present in a module.

For the module declared above, the accompanying overlay is as folllows.
```c
enum { _my_module_configure, _my_module_test };
```
You can see that the items in the `enum` have the same names as the functions in the module, but prefixed with an `_`. The order of the items in the `enum` must match the order of the functions as they are declared in the jumptable.

---

### lf_module

The final step taken to prepare a module for accessibility by the message runtime is to declare its `lf_module` object. The `lf_module` object can be declared manually using the following preprocessor macro.
```c
LF_MODULE(_my_module, "my_module", "Performs operations using x, y, and z.", -1);
```
This syntax creates an `lf_module` structure called `_my_module` and populates it with the module's plaintext name and description, as well as the module's index. At the time of the module's declaration, however, this index is not known, and is thus set to `-1`. If the index is not set to `-1`, undefined behavior could result if an invocation were to be performed on the module.

> **Note:** It is much safer to create modules using the [package manager](./package-manager.html). The package manager handles the creation of the `lf_module` structure automatically, and provides a template for the module's virtual interface and overlay. The package manager also provides warnings about modules that may be incorrectly defined.

Once the lf_module structure has been defined, it needs to be bound to a module that has been loaded onto the device.
