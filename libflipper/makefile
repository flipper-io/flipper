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

else ifeq ($(target),linux)

output = libflipper.so

ifeq ($(architecture),x86_64)
objformat = elf64
else
objformat = elf32
endif

libraries = -lusb

else
$(error "The operating system you are running is not supported by this build script.")
endif

# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #
cc = $(shell which 'clang')
nasm = $(shell which 'nasm')
rsync = $(shell which 'rsync')
ln = $(shell which 'ln')

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

install:

	# ~ Create the SDK and its subdirectories if they do not already exist. ~ #
	mkdir -p $(PREFIX)/flipper/lib $(PREFIX)/flipper/include $(PREFIX)/flipper/resources

	# ~ Copy the library into the SDK's library directory. ~ #
	$(rsync) -r $(output) $(PREFIX)/flipper/lib/$(output)

	# ~ Symlink the library into the system's library directory. ~ #
	$(sudo) $(ln) -sf $(PREFIX)/flipper/lib/$(output) $(PREFIX)/lib/$(output)

	# ~ Symlink the include directory into the system's include directory. ~ #
	$(sudo) $(ln) -sfn $(PREFIX)/flipper/include $(PREFIX)/include/flipper

	# ~ Symlink the master header file into the system's include directory. ~ #
	$(sudo) $(ln) -sf $(PREFIX)/flipper/include/flipper.h $(PREFIX)/include

	# ~ Copy the header files into the SDK's include directory. ~ #
	$(rsync) -r ../include/* $(PREFIX)/flipper/include --exclude 'flipper'

	# ~ Copy the special header files into the SDK's include directory. ~ */
	$(rsync) -r ../include/flipper/* $(PREFIX)/flipper/include


clean:

	rm -rf $(shell find . -follow -name '*.o' -or -name '$(output)')
