# Application

This example demonstrates how to build an application for the device. An
application is standalone code that runs on the hardware without intervention
from a host computer. This example application blinks an LED that is attached
to `IO_1`.

#### Steps to Follow

Install libflipper.
```sh
make libflipper
```

#### Build the Application

Build the application
```sh
make
```
If using FVM open a seperate terminal window and run.
```sh
fvm
```
If working with FVM be sure to replace the following line inside your code. 
```
flipper_attach();
```
with 
```
struct _lf_device *fvm = flipper_attach_network("localhost");
```

#### Run the Application

```sh
make install
```

By running `make install` the application will be loaded onto the attached
device and start executing immediately. If there is an LED attached to `IO_1`, it will begin to blink.

If working with FVM, observe FVM print out
```
led_rgb(x, y, z)
```