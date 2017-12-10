# Flipper

### Our Goal

Flipper is an embedded development platform that can be controlled from applications written for any platform using any programming language. Flipper accomplishes this by exposing functions that live on the hardware as methods or functions that can be called from applications running on a host device. Applications can also be developed to run directly on the hardware. Simply by marking each C function with `LF_FUNC`, our tools will automatically export device functions as a library that can be used from a higher level context.

---

### Quickstart

To build Flipper, you first need to install the project dependancies:

`gcc`, `pkg-config`, `rust`, `libusb-1.0`, `avr-gcc`, `avr-libc`, `dfu-programmer` `arm-none-eabi-gcc`, `newlib`

Once you've installed these dependancies, which should be as simple as `apt-install` or `brew install`, you should be able to build everything. If you don't want to install the toolchains needed to build the embedded operating system, you can optionally build only the platform library or the utils.

|       Target      |                            Description                       |
|:-----------------:|:------------------------------------------------------------:|
| `make libflipper` | Builds the platform library, `libflipper`.                    |
| `make atmegau2`   | Builds the firmware for the microcontroller.                 |
| `make atsam4s`    | Build the embedded operating system for the microprocessor.  |
| `make utils`      | Builds the utilities needed to flash and debug the hardware. |

> All of the intermediates are placed in the `build` directory.

Once you've built, you can install the platform library and tools.

```
make install
```
> The default `PREFIX` for installation is `/usr/local/`. If you wish to change the prefix, set the prefix before the install like `PREFIX=/path/to/prefix make install`

If you are on Linux, you will need to install the `udev` rule that lets `libflipper` talk to the hardware. You will then need to trigger a rule reload:

```
cp assets/99-flipper.rules /etc/udev/rules.d
udevadm trigger
```

If you have hardware attached, you can flash the microcontroller:

```
make install-atmegau2
```

And flash the microprocessor:

```
make install-atsam4s
```

As you are making changes, you only need to reflash the device that has had its firmware image changed. If you're making changes to the microcontroller firmware in `carbon/atmegau2`, you will need to reflash the microcontroller. If you are making changes to the microprocessor firmware in `carbon/atsam4s`, or the emebdded operating system in `kernel`, you will need to reflash the microprocessor.

---

### Example

Once you've installed the platform library and tools, you can try the example.

```
cd examples/app
make
```

This will build an application for the device. You can then install this application on the device:

```
make install
```

---

### Contribution

Contribution is welcome! Feel free to submit pull requests to this repository with any improvements you make to the codebase. If you are interested in contributing more than a pull request, or would like to discuss hardware contributions, please email us at `opensource@flipper.io`.

---

###### Copyright © 2013-2017 Flipper Engineering - All rights reserved.
