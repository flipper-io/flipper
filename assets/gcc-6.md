## Compiling GCC 6.2 et al for Flipper (F3)

### Get the Sources

Download the latest version of GCC, GDB, Newlib, and Binutils from their respective FTP servers.

As of August 8, 2016, the latest versions of each are as follows:

```
gcc-6.2.0
binutils-2.27
newlib-2.4.0
gdb-7.11
```

### Setup

Before building the toolchain from source, we must provide a few environment variables that tell the configuration scripts where to install the built toolchain, what target to build for, and what the target CPU is. I found that I was unable to build GCC-6.2.0 using the latest version of Clang. Instead, I used my package manager to install GCC-6.1.0, and exported the `CC` and `CXX` environment variables as the absolute paths to the respective tools.

```
export TARGET=arm-none-eabi
export CPU=cortex-m4
export CC=`which gcc-6`
export CXX=`which g++-6`
export PREFIX=`pwd`/$TARGET
export PATH=$PATH:$PREFIX/bin
mkdir $TARGET
```

#### Building on Mac OS X El Capitan

A simple brew command can be used to install the appropriate version of GCC as well as the dependancies required to cross compile a toolchain for ARM.

`brew install gcc`

### Building Binutils

Arguably the easiest, this step builds and installs the binary utilites of the toolchain. Due to deprication warnings, the build will fail. In order to combat this we must specify `--disable-werror`.

```
mkdir binutils-build
cd binutils-build
./../binutils-*/configure --prefix=$PREFIX --target=$TARGET
make -j8
make install
cd ..
```

### Building GCC

This step builds and installs GCC using Newlib.

```
mkdir gcc-build
cd gcc-build
ln -s ../newlib-*/newlib .
./../gcc-*/configure --prefix=$PREFIX --target=$TARGET --with-cpu=$CPU --with-mode=thumb --disable-multilib --enable-languages=c,c++,objc --with-newlib
make -j8
make install
cd ..
```

### Building Newlib

This step builds and installs Newlib.

```
mkdir newlib-build
cd newlib-build
./../newlib-*/configure --prefix=$PREFIX --target=$TARGET --with-cpu=$CPU --with-mode=thumb --disable-multilib --with-tune=$CPU --enable-languages=c,c++,objc --disable-newlib-supplied-syscalls
make -j8
make install
cd ..
```

### Building GDB

This step configures and installs GDB.

```
mkdir gdb-build
cd gdb-build
./../gdb-*/configure --prefix=$PREFIX --target=$TARGET
make -j8
make install
cd ..
```