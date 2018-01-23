# Flipper

[![Build Status](https://travis-ci.com/georgemorgan/flipper.svg?token=mWpYCFjw1S5VKLVNvZFo&branch=master)](https://travis-ci.com/georgemorgan/flipper)

<div style="text-align:center"><img src ="https://www.flipper.io/assets/img/landing.png" style="width: 200px;"/></div>

## About

Flipper is a development platform that offers a new take on the traditional embedded software development cycle.

Using Flipper, a developer creates and debugs program logic on a development machine instead of deploying code onto an embedded device and debugging it there. Our library performs remote procedure calls to a connected device instead of emulating the hardware. This makes it possible to use real hardware to test program behavior in real time. When development is complete, the project can be cross compiled and loaded onto a Flipper device for native performance.

This new embedded development workflow makes it simple to use widely adopted and industry standard tools, like Xcode and Visual Studio, to develop and debug applications that interact with embedded hardware peripherals. The capabilities of the platform extend beyond the scope of embedded software development; Flipper makes it easy to write applications in any programming language, on any platform, that control real hardware.

To purchase a Flipper device, please visit our store [here](https://www.flipper.io/buy).

## Quickstart

### Install the dependancies

To build Flipper, you first need to install the project dependancies. To build firmware images for the device you will need to install two GNU cross compilers: `avr-gcc` and `arm-none-eabi-gcc`. To build and use libflipper you will need to install `libusb-1.0`. To build and install the console you will need to install the `rust` compiler and tools. To write firmare images to the device, you will need `dfu-programmer`. You may selectively install dependancies depending on what you wish to contribute to.

#### [Homebrew](https://brew.sh/)
```
brew tap osx-cross/avr osx-cross/arm
brew install rust libusb avr-gcc dfu-programmer arm-gcc-bin
```

#### APT
```
apt-get install build-essential libusb-1.0-0-dev
apt-get install dfu-programmer avr-libc binutils-avr gcc-avr
apt-get install libnewlib-arm-none-eabi binutils-arm-none-eabi gcc-arm-none-eabi
```

### Clone and build the repository
```
git clone https://github.com/georgemorgan/flipper.git
cd flipper
make
```

To selectively build components of the project, see the table below.

|      Target     |                      Description                      |
|-----------------|-------------------------------------------------------|
| `make libflipper` | Builds the [`library`](./library) that talks to the hardware. |
| `make console` | Builds the [`console`](./console) tool that creates and manages projects. |
| `make atmegau2` | Builds the firmware for the [`microcontroller`](./carbon/atmegau2). |
| `make atsam4s`| Build the embedded operating system for the [`microprocessor`](./carbon/atsam4s). |
| `make utils` | Builds the [`utilities`](./utils) needed to flash and debug the hardware. |

> All of the intermediates are placed in the `build` directory.

### Install the tools

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

## Updating the Firmware

Once you get everything installed, you should update your board's firmware. Attach only one Flipper device to your computer via USB, and then run the following command.

```
make update
```

## Example

Once you've installed the platform library and tools, you can try the example.
```
cd examples/app
make
```

This will build an application for the device. You can then install this application onto the attached device.
```
make install
```

See the README for each example for details.

## Contribution

Contribution is welcome! Feel free to submit pull requests to this repository with any improvements you make to the codebase. If you are interested in contributing more than a pull request, or would like to discuss hardware contributions, please email us at `opensource@flipper.io`.

For more information regarding contribution, please see [CONTRIBUTING](./CONTRIBUTING.md).

## License

Flipper is distributed under the Apache License (Version 2.0).

See [LICENSE](./LICENSE) for details.

---

###### Copyright © 2013-2017 Flipper Engineering - All rights reserved.
