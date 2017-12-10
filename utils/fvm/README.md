# fvm

Flipper Virtual Machine (FVM) creates a virtual device on `localhost`. This virtual device can then be attached to as a network device.

FVM is primarily used for debugging the runtime, event system, and other components that expect a valid flipper device to be attached, but don't yet have supporting hardware drivers to use a real Flipper device.

FVM takes no command line arguments. To start a virtual machine, just start the program

```
fvm
```

and then attach to it from your app.

```
struct _lf_device *fvm = flipper_attach_network("localhost");
```

### Modules

FVM is also fully capible of loading Flipper modules in the form of a dynamically linked library. An FVM app can be built by changing the `TARGET` of an application build to `fvm`. Applications can be loaded by providing their paths as arguments to the FVM program.

```
fvm /path/to/app1.so /path/to/app2.so [...]
```