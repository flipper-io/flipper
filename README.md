# What is the Flipper Toolbox?

The Flipper Toolbox is a repository of sofware components that work together enable control of Flipper hardware from a variety of different host platforms and programming environments.

Interfacing with embedded hardware generally requires a substantial amount of overhead. Simple control schemas that deliver data from dedicated hardware to a host for processing are difficult to implement reliably and are often extremely limited in scope.

This overhead distances the consumer from the hardware that they are working with, and, in turn, distances them from their projects. Flipper is an embedded electronics prototyping platform that enables developers to work at the bare metal, while simultaneously taking advantage of a highly sophistocated but lightweight control stack known as the Flipper Toolbox. The toolbox comes with a suite of development tools that can be used to interact with Flipper hardware from a wide range of platforms. For example, the component `FlipperiOS.framework` enables complete hardware control of Flipper from Apple's iOS devices. The toolbox also allows Flipper to be controlled from different programming environments such as Python, Java, Swift, Objective-C, and more.

Flipper hardware is limited only by the Flipper Toolbox. Flipper works natively with USB, but backpacks exist that enable the toolbox to interact with Flipper over Wi-Fi and Bluetooth. The toolbox can be ported any platform with a standard C library and configured to communicate with Flipper over almost any bus architecture.

This document provides a brief overview of the major components of the Flipper Toolbox. Specifics pertaining to each component can be found in the README of the respective component's root directory.

-

### Flipper Console

The Flipper Console is a powerful tool that can be used to access Flipper hardware without first writing or compiling code of any kind.

Usage: `flipper object function [arguments ...]`

Example: `flipper io write 0 true`

## 

### libflipper

At the heart of the Flipper toolbox is a humble dynamically linked library by the name of `libflipper`. This library is responsible for handling communications between a host computer and Flipper hardware, as well as taking care of the behind-the-scenes miscellany needed to ensure that Flipper is doing its job.

## Supported IDEs

The Flipper Toolbox is supported across a variety of different IDEs.

- **Processing**
- **Xcode**
- **Clion**
- **IntelliJ**

## Language Wrappers

### python

The `python` sub-project is a language wrapper for Flipper which bootstraps the functionality of `libflipper` to the Python programming language. Compiling this sub-project will yield a directory of shared objects each of which can be imported into Python and used as a class. All of these modules can be imported at once by using `from flipper import *`.

**Example Code**

`led.rgb(0, 0, 128)`

### java

The `java` sub-project is a language wrapper for Flipper which bootstraps the functionality of `libflipper` to the Java programming language. Compiling `java` yields two output files: `libflipperjava.so` and `flipper.jar`. The `jar` file can be compiled against any Java class and imported using the standard `import com.domain.name.*` syntax.

**Example Code**

`flipper.led.rgb(0, 0, 128)`

### cpp

The `cpp` sub-project wraps libflipper functionality to the C++ programming language.

**Example Code**

```
FTLED led;

led.rgb(0, 0, 128);
```

### obj-c

The `obj-c` language wrapper wraps `libflipper` to Objective-C classes.

**Example Code**

```
FTLED *led = [[FTLED alloc] initWithDevice:[FTDevice deviceWithName:"elroy"]];

[led setR:0, g:0 b: 128];
```

### swift

The `swift` language wrapper wraps `libflipper` to Swift classes.

**Example Code**

```
let led = flipper.led()

led.rgb(0, 0, 128)
```

### wiring

The `wiring` sub-project is yet another language wrapper for `libflipper` that abstracts calls to Flipper hardware behind an API that will be very familiar to Arduino users.

```
pinMode(1, OUTPUT);

digitalWrite(0, HIGH);
```

## Prebuilt Frameworks for your Convienience

### Apple

Include any of the below in Xcode, and any of these devices can control Flipper.

#### `FlipperiOS.framework`

Control Flipper from any iOS device.

#### `FlipperWatchOS.framework`

Control Flipper from the Apple Watch.

#### `FlipperTvOS.framework`

Control Flipper from the Apple TV.

### Android

#### `FlipperAndroid.jar`

Control Flipper from any Android phone or Tablet.

-
### Legal Notice

**This repository contains the entirety of the software infrastructure needed to take full advantage of Flipper and its hardware components. All material found in this repository is strictly private. Sharing details about its implementation is punishable by law.**

Copyright © Flipper Engineering 2015-2016 • All Rights Reserved