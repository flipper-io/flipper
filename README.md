# Introducing the Flipper Toolbox

**This repository contains the entirety of the software infrastructure needed to take full advantage of Flipper and its hardware components.**

###### All material found in this repository is strictly private. Sharing details about its implementation is punishable by law.

Copyright © Flipper Engineering 2015-2016 • All Rights Reserved

# Components of the Toolbox

Listed below are the major components of the toolbox and a brief description of how they relate to the Flipper Toolbox as a whole.

### console

The Flipper Console is a powerful tool that can be used to access Flipper hardware without first writing or compiling code of any kind.

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