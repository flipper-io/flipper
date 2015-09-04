# Introducing the Flipper Toolbox

**This repository contains the entirety of the software infrastructure needed to take full advantage of Flipper and its hardware components.**

###### All material found in this repository is strictly private. Sharing details about its implementation is punishable by law.

Copyright © Flipper Engineering 2015-2016 • All Rights Reserved

# Components of the Toolbox

### console

The Flipper Console is a powerful tool that can be used to access Flipper hardware without first writing or compiling code of any kind.

## 

### libflipper

At the heart of the Flipper toolbox is a humble dynamically linked library by the name of `libflipper`. This library is responsible for handling communications between a host computer and Flipper hardware, as well as taking care of the behind-the-scenes miscellany needed to ensure that Flipper is doing its job.

## 

### Java

The `java` sub-project is a language wrapper for Flipper which bootstraps the functionality of `libflipper` to the Java programming language. Compiling `java` yields two output files: `libflipperjava.so` and `flipper.jar`. The `jar` file can be compiled against any Java class and imported using the standard `com.domain.name.*` syntax.

##### Building

`cd java && make`

##### Installing

`make install` or `make && mv package $PYTHON_LIB_DIR/flipper` for custom installation.

##### Example Code

``` public static void main(String[] args) { }```

## 

### Python

The `python` sub-project is a language wrapper for Flipper which bootstraps the functionality of `libflipper` to the Python programming language. Compiling this sub-project will yield a directory of shared objects each of which can be imported into Python and used as a class. All of these modules can be imported at once by using `from flipper import *`.

##### Building

`cd python && make`

##### Installing

`make install`

##### Example Code

```from flipper import *```

## 
