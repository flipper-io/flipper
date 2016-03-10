.PHONY: all install burn clean

# ~ Specifiy the name of the output file. ~ #
output = osmium-atmega.bin

# ~ Specify the long platform name. ~ #
platform = atmega16u2

# ~ Specify the target clock frequency in MHz. ~ #
clock = 16000000

# ~ Specify a compiler and the binary utilities needed to deploy code to the target. ~ #
cc = $(shell which 'avr-gcc')
objcopy = $(shell which 'avr-objcopy')

# ~ Specify uploader and flags. ~ #
uploader = $(shell which 'avrdude')
uploader_flags = -p $(platform) -V -c usbtiny -U lfuse:w:0xFF:m -U hfuse:w:0xD9:m -U flash:w:$(output)

# ~ Specify platform specific preprocessor flags. ~ #
cpu_flags = -DARCH=ARCH_AVR8 -mmcu=$(platform) -D__AVR_$(shell echo $(platform) | tr a-z A-Z)__ -D__atmega_build__

# ~ Declare the preprocessor statements to be used during object generation. ~ #
preprocess = -std=gnu99 -Os $(cpu_flags) -DF_CPU=$(clock)UL -D__osmium__ -Wno-pragmas

# ~ Specify the include directories. ~ #
include_directories = ../../../include ./include

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #
include rules.make

all: clean

	# ~ Link the object files together to produce an output file. ~ #
	$(cc) $(prefix) $(ldflags) $(sort $(objects)) -o $(basename $(output)).elf

	# ~ Convert the 'elf' file into a raw binary. ~ #
	$(objcopy) -O binary $(basename $(output)).elf $(output)

install:

	# ~ Create the install directory if it doesn't exist. ~ #
	mkdir -p $(PREFIX)/flipper/osmium

	# ~ Copy the binary into the SDK's firmware directory. ~ #
	$(rsync) -r $(output) $(PREFIX)/flipper/osmium/$(output)

burn:

	# ~ Flash the connected device with the binary using the appropriate uploader. ~ #
	$(uploader) $(uploader_flags)

clean:

	rm -rf $(shell find . -follow -name '*.o' -or -name '*.elf' -or -name '*.bin')
