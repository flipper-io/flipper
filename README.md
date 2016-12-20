# Flipper Toolbox

### Overview

Flipper is an embedded development platform designed from the ground up to
provide seamless abstraction between the application layer and common hardware
peripherals such as GPIO, I2C, SPI, UART, and more. In other words, Flipper makes it easy to control **simple hardware** from **any programming language** on **any platform** in **real-time**.

This "Flipper Toolbox" is a repository of complimentary sofware components designed to work together to enable control Flipper hardware from a wide variety of host platforms and programming environments. This repository is organized into a variety of submodules pertainent to the differing layers of abstraction mentioned above. More on each of these submodules can be found within the `README` of the submodule.

### Building and Installation

> ⚠️ **Warning:** Python (>= 2.5) must be installed prior to building the toolbox.

```
./waf configure build install
```
#### Details

The **toolbox** uses the [**Waf**](https://github.com/waf-project/waf) build
system. *"Waf is a Python-based framework for configuring, compiling and
installing applications."* This repository ships with an executable Waf script tha can be used to configure, build, and install the toolbox.

##### Setup

The first phase of executable generation is called configuration wherein Waf determines which system it is being run on and aggregates the paths to the dependancies it needs to process all recipes of the build. This can be
accomplished using the following command.

```
./waf configure
```

Any platform specific dependancies not found will be listed duing the
configuration phase. A list of the major dependancies can be found below.

```
python
arm-none-eabi-gcc
arm-none-eabi-objcopy
arm-none-eabi-newlib
avr-gcc
avr-objcopy
avr-libc
dfu-programmer
ghc
stack
libusb-1.0
```

##### Building

Following configuration, compilations can be performed simply by
executing the Waf script.

```
./waf
```

##### Installing

After the configuration and build phases, the toolbox can be installed to the sytem using the following command.

```
./waf install
```