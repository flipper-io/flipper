# Inboke this file using 'make -C toolchain.make all'

TARGET = arm-none-eabi
CPU = cortex-m4
ROOT = $(shell pwd)
PREFIX = $(ROOT)/$(TARGET)

CC := gcc-7
CXX := g++-7

# Use the GNU tools on Mac
ifeq ($(shell uname -s),Darwin)
LIBTOOL = glibtool
LIBTOOLIZE = glibtoolize
SED = gsed
endif

# The versions of the dependancies to be used.
GMP = gmp-6.1.2
MPFR = mpfr-4.0.0
MPC = mpc-1.1.0
ISL = isl-0.19
LIBELF = libelf-0.8.13
BINUTILS = binutils-2.30
GCC = gcc-7.3.0
NEWLIB = newlib-3.0.0
GDB = gdb-8.0.1

FLAGS = --target=$(TARGET) --with-cpu=$(CPU) --with-mode=thumb --disable-shared --enable-static --disable-multilib --disable-libssp --disable-nls --with-gnu-as --with-gnu-ld --disable-threads --without-headers --with-newlib --with-gmp=$(PREFIX)/gmp --with-mpfr=$(PREFIX)/mpfr --with-mpc=$(PREFIX)/mpc --with-isl=$(PREFIX)/isl --with-libelf=$(PREFIX)/libelf

.PHONY: all clean

all: configure binutils gcc gdb

clean:
	rm -rf *-build

configure:
	mkdir -p $(PREFIX)

install:
	rsync -avr $(TARGET)/* /usr/local/$(TARGET)

gmp:
	mkdir -p gmp-build
	cd gmp-build && $(ROOT)/$(GMP)/configure --disable-shared --enable-static --prefix=$(PREFIX)/gmp
	$(MAKE) -C gmp-build -j8 all
	$(MAKE) -C gmp-build -j8 check
	$(MAKE) -C gmp-build -j8 install

mpfr: gmp
	mkdir -p mpfr-build
	cd mpfr-build && $(ROOT)/$(MPFR)/configure --disable-shared --enable-static --prefix=$(PREFIX)/mpfr --with-gmp=$(PREFIX)/gmp
	$(MAKE) -C mpfr-build -j8 all
	$(MAKE) -C mpfr-build -j8 check
	$(MAKE) -C mpfr-build -j8 install

mpc: gmp mpfr
	mkdir -p mpc-build
	cd mpc-build && $(ROOT)/$(MPC)/configure --disable-shared --enable-static --prefix=$(PREFIX)/mpc --with-gmp=$(PREFIX)/gmp --with-mpfr=$(PREFIX)/mpfr
	$(MAKE) -C mpc-build -j8 all
	$(MAKE) -C mpc-build -j8 check
	$(MAKE) -C mpc-build -j8 install

isl: gmp
	mkdir -p isl-build
	cd isl-build && $(ROOT)/$(ISL)/configure --disable-shared --enable-static --prefix=$(PREFIX)/isl --with-gmp-prefix=$(PREFIX)/gmp
	$(MAKE) -C isl-build -j8 all
	$(MAKE) -C isl-build -j8 check
	$(MAKE) -C isl-build -j8 install

libelf:
	mkdir -p libelf-build
	cd libelf-build && $(ROOT)/$(LIBELF)/configure --disable-shared --enable-static --prefix=$(PREFIX)/libelf
	$(MAKE) -C libelf-build -j8 all
	$(MAKE) -C libelf-build -j8 install

binutils:
	mkdir -p binutils-build
	cd binutils-build && $(ROOT)/$(BINUTILS)/configure --prefix=$(PREFIX) $(FLAGS) --disable-werror
	$(MAKE) -C binutils-build -j8 all
	$(MAKE) -C binutils-build -j8 install

gcc-1: gmp mpfr mpc isl libelf
	mkdir -p gcc-build
	cd gcc-build && $(ROOT)/$(GCC)/configure --enable-languages=c $(FLAGS) --prefix=$(PREFIX)
	$(MAKE) -C gcc-build -j8 all
	$(MAKE) -C gcc-build -j8 install

newlib: gcc-1
	mkdir -p newlib-build
	cd newlib-build && $(ROOT)/$(NEWLIB)/configure --target=$(TARGET) -disable-newlib-supplied-syscalls --prefix=$(PREFIX)
	$(MAKE) -C newlib-build -j8 all
	$(MAKE) -C newlib-build -j8 install

gcc: newlib gmp mpfr mpc isl libelf
	mkdir -p gcc-build
	cd gcc-build && $(ROOT)/$(GCC)/configure --enable-languages=c $(FLAGS) --prefix=$(PREFIX)
	$(MAKE) -C gcc-build -j8 all
	$(MAKE) -C gcc-build -j8 install

gdb:
	mkdir -p gdb-build
	cd gdb-build && $(ROOT)/$(GDB)/configure $(FLAGS) --prefix=$(PREFIX)
	$(MAKE) -C gdb-build -j8 all
	$(MAKE) -C gdb-build -j8 install
