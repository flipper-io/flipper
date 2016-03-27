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

# ~ Define target specific flags for chown. ~ */
chown_args = $(shell whoami)

else ifeq ($(target),linux)

output = libflipper.so

ifeq ($(architecture),x86_64)
objformat = elf64
else
objformat = elf32
endif

libraries = -lusb

chown_args = $(shell whoami)

else
$(error "The operating system you are running is not supported by this build script.")
endif

PREFIX ?= /usr/local

# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #
cc = $(shell which 'clang')
nasm = $(shell which 'nasm')
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

	# This part sucks, figure out a better way to do it later:
	$(sudo) cp $(output) $(PREFIX)/lib/
	$(sudo) chown -R $(chown_args) $(PREFIX)/lib/$(output)
	$(sudo) chmod -R 0755 $(PREFIX)/lib/$(output)

	$(sudo) mkdir -p $(PREFIX)/include/flipper

	$(sudo) cp -r ../include/* $(PREFIX)/include/

	$(sudo) chown -R $(chown_args) $(PREFIX)/include/flipper
	$(sudo) chmod -R 0755 $(PREFIX)/include/flipper

clean:

	rm -rf $(shell find . -follow -name '*.o' -or -name '$(output)')
