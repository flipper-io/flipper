# Package Example

This example demonstrates how to build a package. A package is a collection of functions that are installed on the device and can be remotely invoked from any device with libflipper installed.

#### Steps to Follow

Install libflipper.
```sh
make libflipper
```

### Build the Package
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


### Install the Package
```
make install
```
The executable code will now be on the attached Flipper device.

If using FVM then run
```
fvm path/to/module.so
```
FVM will now return results in the terminal window. These results can be analyzed in order to verify the programming pipleline and ensure that the package is working correctly. 

## Running

A package doesn't have an entry point or main function, so no code is run when
it is loaded. Instead, code is run when a call is made to a function that
exists inside the package.

This example provides a test program in the `host` folder. This program calls
the `my_func` function and is built for the host. To execute the function
`my_func` on the device, simply call the host program.

```
./build/host/example
```

If you have an LED attached to `IO_1` on the board, it will change state each
time the test program is run. The host program is remotely invoking the
function that does this on the attached device.
