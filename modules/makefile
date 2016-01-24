output = bme280.a

# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #

localcc = $(shell which 'gcc')

remotecc = $(shell which 'arm-elf-gcc-4.7')

rsync = $(shell which 'rsync')

ln = $(shell which 'ln')

# ~ Declare the preprocessor statements to be used during object generation. ~ #

preprocess = -std=c99 -fpic

# ~ Declare the include directories needed for compilation. ~ #

include_directories = $FLIPPERSDK/include

all: clean

	$()

clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf") $(output)
