# ~ Determine the architecture. ~ #
architecture = $(shell echo $(shell uname -m) | tr A-Z a-z)

# ~ Determine the target. ~ #
target = $(shell echo $(shell uname -s) | tr A-Z a-z)

# ~ Set target specific macros. ~ */
ifeq ($(target),darwin)

# ~ Specifiy the name of the output file. ~ #
output = libflipper.dylib

# ~ Specify object file format for nasm. ~ #
objformat = macho64

# ~ Declare the library dependancies neededed for linking. ~ #
libraries = -framework CoreFoundation -framework IOKit

install_opts =

else ifeq ($(target),linux)

output = libflipper.so

install_opts = -g root -m 0755 -o root -T

ifeq ($(architecture),x86_64)
objformat = elf64
else
objformat = elf32
endif

libraries = -lusb

else
$(error "The operating system you are running is not supported by this build script.")
endif

PREFIX ?= /usr/local

# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #
cc = $(shell which 'clang')
nasm = $(shell which 'nasm')
install = $(shell which 'install')
ln = $(shell which 'ln')
sudo = $(shell which 'sudo')

# ~ Declare the preprocessor statements to be used during object generation. ~ #
preprocess = -std=c99 -fpic -D__verbose__ -Wno-\#pragma-messages

# ~ Pass flags to find so that it only isolates files pertainant to the target platform. ~ #
findflags = \( \( ! -wholename "./targets/*" -and ! -wholename "./architectures/*" \) -or \( -wholename "./targets/$(target)/*" -or -wholename "./architectures/$(architecture)/*" \) \)

# ~ Declare the include directories needed for compilation. ~ #
include_directories = ../include ./include

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #
include rules.make

all: clean

	# ~ Link the object files together to produce an output file. ~ #
	$(cc) -shared -rdynamic $(prefix) $(sort $(objects)) -o $(output) $(libraries)

install: all

	# ~ Copy the library into the SDK's library directory. ~ #
	$(sudo) $(install) $(install_opts) $(output) $(PREFIX)/lib/$(output)

	# This part sucks, figure out a better way to do it later:
	$(sudo) mkdir -p $(PREFIX)/include/flipper

	$(sudo) cp -r ../include/* $(PREFIX)/include/flipper

	$(sudo) chown -R root:root $(PREFIX)/include/flipper
	$(sudo) chmod -R 0755 $(PREFIX)/include/flipper

clean:

	rm -rf $(shell find . -follow -name '*.o' -or -name '$(output)')
