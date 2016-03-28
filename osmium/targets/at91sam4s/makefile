.PHONY: all install burn clean

# ~ Specifiy the name of the output file. ~ #
output = osmium-sam4s.bin

# ~ Specify the target clock frequency in MHz. ~ #
clock = 48054850

# ~ Specify a compiler and the binary utilities needed to deploy code to the target. ~ #
cc = $(shell which 'arm-elf-gcc')
objcopy = $(shell which 'arm-elf-objcopy')

# ~ Specify uploader and flags. ~ #
uploader = $(shell which 'flipper')
uploader_flags = flash $(output)

# ~ Specify platform specific preprocessor flags. ~ #
cpu_flags = -mcpu=arm7tdmi -mtune=arm7tdmi -mthumb -mthumb-interwork -nostartfiles -ffreestanding -fdata-sections -ffunction-sections

# ~ Declare the preprocessor statements to be used during object generation. ~ #
preprocess = -std=gnu99 -Os $(cpu_flags) -DF_CPU=$(clock)UL -D__osmium__ -Wno-pragmas

# ~ Specify the include directories. ~ #
include_directories = ../../../include ./include

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #
include rules.make

all:

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
