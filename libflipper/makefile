# ~ Specifiy the name of the output file. ~ #

output = libflipper.so

# ~ Specify the target platform. ~ #

target = linux

# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #

cc = $(shell which 'gcc')

rsync = $(shell which 'rsync')

ln = $(shell which 'ln')

# ~ Declare the preprocessor statements to be used during object generation. ~ #

preprocess = -std=c99 -fpic -D__verbose__ # -DOVERRIDE_USB

# ~ Pass flags to find so that it only isolates files pertainant to the target platform. ~ #

findflags = \( ! -wholename "./platforms/*" -or -wholename "./platforms/$(target)/*" \)

# ~ Declare the include directories needed for compilation. ~ #

include_directories = ../include

# ~ Declare the library dependancies neededed for linking. ~ #

libraries = -lusb # -framework CoreFoundation -framework IOKit

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #

include rules.make

all: clean

	# ~ Link the object files together to produce an output file. ~ #

	$(cc) -shared -rdynamic $(prefix) $(sort $(objects)) -o $(output) $(libraries)

install:

	sudo mkdir -p $(FLIPPERSDK)/lib $(FLIPPERSDK)/include

	sudo chown -R $(shell 'whoami') $(FLIPPERSDK) 

	# Copy the library into the SDK's library directory.

	$(rsync) -r $(output) $(FLIPPERSDK)/lib/$(output)

	# Symlink the library into the system's library directory.

	sudo $(ln) -sf $(FLIPPERSDK)/lib/$(output) /usr/local/lib/$(output)

	sudo $(ln) -sf $(FLIPPERSDK)/include /usr/local/include/flipper

	sudo $(ln) -sf $(FLIPPERSDK)/include/flipper/flipper.h /usr/local/include

	$(rsync) -r ../include/* $(FLIPPERSDK)/include

clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf") $(output)
