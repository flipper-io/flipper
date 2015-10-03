# ~ Specifiy the name of the output file. ~ #

output = flipper

# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #

cc = $(shell which 'gcc')

rsync = $(shell which 'rsync')

# ~ Declare the preprocessor statements to be used during object generation. ~ #

preprocess = -std=c99

# ~ Declare the library dependancies neededed for linking. ~ #

libraries = -L "../libflipper" -lflipper

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #

include rules.make

libflipper:
        
	$(MAKE) -C "../libflipper" all install

all: libflipper

	# ~ Link the object files together to produce an output file. ~ #

	$(cc) $(prefix) $(sort $(objects)) -o $(output) $(libraries)

install:

	# ~ Create the SDK's binary directory if it does not already exist. ~ #

	mkdir -p $(FLIPPERSDK)/bin

	# ~ Copy the executable into the SDK's binary directory. ~ #

	$(rsync) -r $(output) $(FLIPPERSDK)/bin

clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf") $(output)
