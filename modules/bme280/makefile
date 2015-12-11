# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #

cc = $(shell which 'gcc')

rsync = $(shell which 'rsync')

ln = $(shell which 'ln')

# ~ Declare the preprocessor statements to be used during object generation. ~ #

preprocess = -std=c99 -fpic

findflags = \( ! -wholename "*/remote/*" \)

# ~ Declare the include directories needed for compilation. ~ #

include_directories = ../include include

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #

include rules.make

all: clean

clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf") $(output)
