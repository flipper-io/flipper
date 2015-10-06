# ~ No sudo? ~ #

sudo =

# ~ Specifiy the name of the output file. ~ #

output = hal.fvm

# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #

cc = $(shell which 'gcc')

rsync = $(shell which 'rsync')

ln = $(shell which 'ln')

# ~ Declare the preprocessor statements to be used during object generation. ~ #

preprocess = -std=c99 -fpic -D__verbose__

# ~ Pass flags to find so that it only isolates files pertainant to the target platform. ~ #

findflags = \( ! -wholename "./platforms/*" -or -wholename "./platforms/$(target)/*" \)

# ~ Declare the include directories needed for compilation. ~ #

include_directories = ../include

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #

include rules.make

all: clean

	# ~ Link the object files together to produce an output file. ~ #

	$(cc) -shared -rdynamic $(prefix) $(sort $(objects)) -o $(output)

clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf") $(output)
