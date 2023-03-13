# fvm

Flipper Virtual Machine (FVM) is a virtual machine, which is a way to say that it's a flipper board that runs on your host computer. You can attach to it like you can a real board. It makes testing much easier since there's no hardware in the loop to fail.

FVM creates a virtual device on `localhost`. This virtual device can then be attached to as a network device.

FVM is primarily used for debugging the runtime, event system, and other components that expect a valid flipper device to be attached, but don't yet have supporting hardware drivers to use a real Flipper device.

When you attach to FVM via libflipper, you are calling a remote stub on the device. The stubs are exposed by adding the `LF_FUNC` macro to the function's declaration. This is what a "module" is. It's a dynamically linked library `(.so)` of these stubs.

If you want to call stubs in a module using FVM, you have to have the dynamic loader bring those symbols into memory. That's what the arguments to FVM do. (i.e `fvm my_lib.so` will dynamically load all those stubs into FVM so that they can be called from libflipper.

To start a virtual machine and begin working with FVM open a termianl window and type
```
fvm
```
In order to test an application add the following line of code in the applicaton.
```
struct _lf_device *fvm = flipper_attach_network("localhost");
```

### Modules

FVM is also fully capible of loading Flipper modules in the form of a dynamically linked library. An FVM app can be built by changing the `TARGET` of an application build to `fvm`. Applications can be loaded by providing their paths as arguments to the FVM program.

```
fvm /path/to/app1.so /path/to/app2.so [...]
```
Examples of working with FVM can be found in the examples folder.