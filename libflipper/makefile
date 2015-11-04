# ~ No sudo? ~ #

sudo = sudo

# ~ Determine the target. ~ #

target = $(shell echo $(shell uname -s) | tr A-Z a-z)

# ~ Set target specific macros. ~ */

ifeq ($(target),darwin)

	# ~ Specifiy the name of the output file. ~ #

	output = libflipper.dylib

	# ~ Declare the library dependancies neededed for linking. ~ #

	libraries = -framework CoreFoundation -framework IOKit

else ifeq ($(target),linux)

	# ~ Do the same for the other targets. ~ #

	output = libflipper.so

	libraries = -lusb

else

	# ~ If we have no target, specify an error. ~ #

	$(error "Please specify a target.")

endif

# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #

cc = $(shell which 'gcc')

rsync = $(shell which 'rsync')

ln = $(shell which 'ln')

# ~ Declare the preprocessor statements to be used during object generation. ~ #

preprocess = -std=c99 -fpic -D__verbose__ -m32

# ~ Pass flags to find so that it only isolates files pertainant to the target platform. ~ #

findflags = \( ! -wholename "./platforms/*" -or -wholename "./platforms/$(target)/*" \)

# ~ Declare the include directories needed for compilation. ~ #

include_directories = ../include include

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #

include rules.make

all: clean

	# ~ Link the object files together to produce an output file. ~ #

	$(cc) -shared -rdynamic $(prefix) $(sort $(objects)) -o $(output) $(libraries)

install:

	# ~ Create the SDK and its subdirectories if they do not already exist. ~ #

	$(sudo) mkdir -p $(FLIPPERSDK)/lib $(FLIPPERSDK)/include $(FLIPPERSDK)/resources

	# ~ Give the user the ability to read from and write to these directories. ~ #

	$(sudo) chown -R $(shell 'whoami') $(FLIPPERSDK)

	# ~ Copy the library into the SDK's library directory. ~ #

	$(rsync) -r $(output) $(FLIPPERSDK)/lib/$(output)

	# ~ Symlink the library into the system's library directory. ~ #

	$(sudo) $(ln) -sf $(FLIPPERSDK)/lib/$(output) /usr/local/lib/$(output)

	# ~ Symlink the include directory into the system's include directory. ~ #

	$(sudo) $(ln) -sfn $(FLIPPERSDK)/include /usr/local/include/flipper

	# ~ Symlink the master header file into the system's include directory. ~ #

	$(sudo) $(ln) -sf $(FLIPPERSDK)/include/flipper.h /usr/local/include

	# ~ Copy the header files into the SDK's include directory. ~ #

	$(rsync) -r ../include/* $(FLIPPERSDK)/include --exclude 'flipper'

	# ~ Copy the special header files into the SDK's include directory. ~ */

	$(rsync) -r ../include/flipper/* $(FLIPPERSDK)/include

	# ~ Copy the resources into the SDK directory. ~ #

	$(rsync) -r ../resources/* $(FLIPPERSDK)/resources


clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf") $(output)
