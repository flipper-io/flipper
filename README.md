# Flipper

[![Build Status](https://travis-ci.org/flipper-io/flipper.svg?branch=master)](https://travis-ci.org/flipper-io/flipper)

An embedded development platform that can be controlled remotely from any
programming language. [flipper.io](https://flipper.io)

# Why did we make Flipper?

At the highest level, Flipper is just like any embedded development platform:
it's a tool for using software to interact with and control electronic
hardware. All embedded platforms have limited resources, often orders of
magnitude smaller than those available on a typical desktop computer. These
limitations impose heavy constraints on the type of applications that you can
write for these platforms. For example, nearly all embedded programs are
written in C or C++
([though Rust is getting there](http://www.rust-embedded.org/)), many lack
access to resources like heap memory, and simple tasks like communicating with
other devices can involve a lot of manual protocol implementation. Quite
frankly, embedded development is really hard, especially for beginners. With
Flipper, we're trying to make embedded development easier by moving the
application logic to a more comfortable place - the developer's computer.

# What's special about Flipper?

Flipper has two core components: a lightweight operating system for the
device, and a library to install on "host machines", such as the developer's
computer. Together, these form a system that we like to call the Flipper
Message Runtime, or FMR. FMR allows you to write programs that execute on the
host machine but directly control the hardware. I like to think of it as
hardware-as-a-library. This ability to remotely execute functions on the
device from a host machine has far-reaching implications, and opens up a
whole new world of possibilities for embedded projects. The most exciting
feature of Flipper is the ability to control it from any platform (Linux,
MacOS, Windows, Android, or iOS), using any programming language.

## An example

Flipper has a concept of "packages", which are just compiled code that live
on the device. The simplest package is the LED, and looks something like this:

```c
// led.c
void led_rgb(int r, int g, int b) {
    // Twiddle some bits, set the LED color
}
```

Every package can have language bindings generated for it. This means that
once we have `led.c`, we can get `led.py` for free:

```py
# led.py
from Flipper import led
led.rgb(0, 255, 0); # Sets the LED to green!
```

Any language that can execute C under the hood (pretty much all of them!) is
capable of controlling Flipper like this. So far we're well underway to support
C/C++, Rust, Python, and Swift, and we plan on adding Objective-C, Java, and
Javascript in the near future.

# How it works

Let's go through a brief tour of Flipper's major components and explain how
they all work together. Hopefully this will help to paint a clearer picture
of what Flipper is capable of.

```
---------------------                        -------------
| Language bindings |                        |  Packages |
---------------------                        -------------
---------------------                        -------------
|    Libflipper     |                        |  Osmium   |
---------------------                        -------------
---------------------                        -------------
|                   |                        |           |
|                   |                        |           |
|       Host        |  USB, Wifi, Bluetooth  |  Flipper  |
|                   |  <~~~~~~~~~~~~~~~~~~>  |           |
|                   |                        |           |
---------------------                        -------------
```

Libflipper is the name of the library which captures function calls on the
host machine and sends them to Flipper. A host machine can be a desktop
computer, smartphone, or even a server. Hosts can communicate with Flipper
using any form of communication, with usb, wifi, and bluetooth being the main
three. In order to use high level programming languages, we have
"language bindings" which make use of libflipper in order to control Flipper.

Osmium is our affectionate name for Flipper's operating system. It's in charge
of communicating with libflipper and receiving instructions, then executing
code in the correct package and returning the result. Flipper comes with
several "standard" packages, which provide the core functionality of the board.
These correspond pretty closely to the peripherals in the hardware, giving
access to GPIO, USART, SPI, I2C, and more. However, Flipper allows you to
write your own packages. Packages always run directly on the device, meaning
that you get native performance, but every package can have language bindings
generated for it, allowing you to execute functions in that package remotely
from your language of choice.

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
make update
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
