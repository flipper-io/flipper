# ~ Specifiy the name of the output file. ~ #

output = libflipper.dylib

# ~ Specify the target platform. ~ #

target = osx

# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #

cc = $(shell which 'clang')

rsync = $(shell which 'rsync')

ln = $(shell which 'ln')

# ~ Declare the preprocessor statements to be used during object generation. ~ #

preprocess = -std=c99 -D__verbose__ # -DOVERRIDE_USB

# ~ Pass flags to find so that it only isolates files pertainant to the target platform. ~ #

findflags = \( ! -wholename "./platforms/*" -or -wholename "./platforms/osx/*" \)

# ~ Declare the include directories needed for compilation. ~ #

include_directories = ../include

# ~ Declare the library dependancies neededed for linking. ~ #

libraries = -framework CoreFoundation -framework IOKit

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #

include rules.make

all: clean

	# ~ Link the object files together to produce an output file. ~ #

	$(cc) -shared -rdynamic $(prefix) $(sort $(objects)) -o $(output) $(libraries)

install:

	mkdir -p $(FLIPPERSDK)/lib

	# Copy the library into the SDK's library directory.

	$(rsync) -r $(output) $(FLIPPERSDK)/lib/$(output)

	# Symlink the library into the system's library directory.

	$(ln) -sf $(FLIPPERSDK)/lib/$(output) /usr/local/lib/$(output)

clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf") $(output)