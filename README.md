# Flipper

[![Build Status](https://travis-ci.org/flipper-io/flipper.svg?branch=master)](https://travis-ci.org/flipper-io/flipper) [![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](https://github.com/flipper-io/flipper/blob/master/README.md#license) [![flipper.io](https://img.shields.io/badge/store-flipper.io-green.svg)](https://flipper.io) [![Chat on discord](https://img.shields.io/badge/chat-on%20discord-7289DA.svg)](https://discord.gg/R5zdK4m) [![Email us](https://img.shields.io/badge/email-opensource%40flipper.io-lightgray.svg)](mailto:opensource@flipper.io)

## About

Flipper is a new kind of embedded development platform that lets developers use familiar tools to create amazing applications that interact with embedded hardware. Flipper can be controlled from [these](./languages) programming langauges running on desktop, mobile, and web applications.

1. **Purchase a board from our web store
   [here](https://flipper.io/products/flipper-carbon-developer-unit).**
    - You can't get started without a board. Pick one up today!
2. **Visit [this](https://github.com/flipper-io/flipper/wiki/Getting-Started)
Wiki page to set up your environment.**
    - Our Wiki is easy to navigate. Start at the top and work your way through it.
3. **Try out the examples [here](./examples).**
    - The best way to learn is by example. Dive head first into our intuitive examples.
4. **Ask questions!**
    - We are building a community around the platform, and we want you to be a part of it. Join our [Discord](https://discord.gg/R5zdK4m) server and ask questions, [email us](mailto:opensource@flipper.io), or open issues here on GitHub.

## Quickstart

### Install the dependencies

To build Flipper, you first need to install the project dependencies. To build
firmware images for the device you will need to install two GNU cross
compilers: `avr-gcc` and `arm-none-eabi-gcc`. To build and use libflipper you
will need to install `libusb-1.0`. To build and install the console you will
need to install the `rust` compiler and tools. To write firmware images to the
device, you will need `dfu-programmer`. You may selectively install dependencies
depending on what you wish to contribute to.

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
apt-get install pkg-config python-pip python-pyelftools
```

#### Rust

In addition to the dependencies installed via apt or brew, you will need to install the
Rust toolchain in order to compile the console. Cross-platform instructions on installing
Rust can be found at [rustup.rs](https://rustup.rs/).

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

If you are on Linux, you will need to install the `udev` rule that lets
`libflipper` talk to the hardware. You will then need to trigger a rule reload:

```
cp assets/99-flipper.rules /etc/udev/rules.d
udevadm trigger
```

## Updating the Firmware

Once you get everything installed, you should update your board's firmware.
Attach only one Flipper device to your computer via USB, and then run the
following command.

```
make install-atmegau2
make install-atsam4s
```

## Example

Once you've installed the platform library and tools, you can try the example.
```
cd examples/app
make
```

This will build an application for the device. You can then install this
application onto the attached device.
```
make install
```

See the README for each example for details.

## Contribution

Contribution is welcome! Feel free to submit pull requests to this repository
with any improvements you make to the codebase. If you are interested in
contributing more than a pull request, or would like to discuss hardware
contributions, please email us at `opensource@flipper.io`.

For more information regarding contribution, please see
[CONTRIBUTING](./CONTRIBUTING.md).

## License

Flipper is distributed under the Apache License (Version 2.0).

See [LICENSE](./LICENSE) for details.

---

###### Copyright © 2013-2018 Flipper Engineering
