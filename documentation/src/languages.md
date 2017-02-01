Languages
===

One of most powerful features of Flipper as an embedded development platform is the ease with which support for the hardware can be bound to new programming languages and environments.

This section of the documentation will illustrate the basic steps that must be taken in order to write language bindings targeting Flipper hardware.

The only prerequisite that is required of the language for which bindings will be written is that it has an **FFI** (Foreign Function Interface) that allows the language to execute C functions within a dynamic library. Many wellsupported languages have built-in FFIs that extend this support to their language. The FFI will enable functions and data structures created in the target language to access`libflipper`, and in turn, Flipper hardware.

Ensure that you understand the mechanisms exposed by [libflipper](./libflipper.html) before attempting to write a language binding.

---

### Attach and Select

As a language binding author, you will want to bind the functionality of the `flipper.attach` and `flipper.select` functions to your language  before proceding to bind any other functionality of `libflipper`. This will enable you to test the ability of your lanugage to communicate with `libflipper` via its FFI, as well as establish a basic structure for how your language understands what device it is connected to.

> **Note:** For reasons described in the [message runtime](./libflipper-fmr.html) section of this documentation, `libflipper` aggregates pointers to the functions within a module inside a data structure defined with the same name as the module. As a side effect, this enables the use of the dot syntax in C to invoke module functions. (e.g `led.rgb(r, g, b)`)
>
> This dot can be replaced with an underscore to obtain the symbol name of a module function. For example, the function `led.rgb` resolves to the symbol `led_rgb`.

In an object oriented language, for example, you may wish to store the device pointer in the object returned by the `attach` method you provide to create said device object. Whenever any of this object's methods are invoked, you will then want to be sure to call `flipper_select` on the device pointer stored previously in the object by `attach` to ensure that messages are routed to the appropriate device.

---

### Modules Bindings

All of the functions exposed to interact with Flipper hardware are done so through the [standard modules](./modules.html). Standard modules come preinstalled on Flipper hardware and can be used to change the state of the various peripherals on the device. A complete language binding will provide a means native to the language of interacting with the standard modules.

#### LED Binding

The first standard module you should bind to is the [led module](./modules-led.html) due to the fact that it is the simplest peripheral to observe functioning correctly. If the binding to the `led` module was written correctly, the LED on the attached device should change colors given different values for the R, G, and B parameters each time the `rgb` function is called.

To bind your language to the `led` module, create a class or data structure in your language that will be used to represent the `led` module, and add the functions `configure` and `rgb` to it. In the bodies of these functions, use the FFI to call into the libflipper functions `led_configure` and `led_rgb`, respectively, with the appropriate parameters passed to each. Also ensure that you have somehow guarenteed that `flipper_select` was called with the appropriate device pointer prior to the invocation of the FFI call to the functions within the `led` module.

#### Other Module Bindings

Writing the bindings to the other standard modules is formulaic. By following the same steps described in the example above, bindings to all of the other standard modules can be made.

---

### Binding to User Modules

The most involved aspect of writing a language binding is integrating the functionality of the [message runtime](./libflipper-fmr.html). Before reading this, read that section to ensure that you understand the terminology used.
