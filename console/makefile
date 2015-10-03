# ~ Specifiy the name of the output file. ~ #

output = flipper

# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #

cc = $(shell which 'gcc')

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

clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf") $(output)
