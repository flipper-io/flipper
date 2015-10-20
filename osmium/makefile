# ~ Set platform specific macros. ~ */

ifeq ($(platform),atmega16u2)

	# ~ Specifiy the name of the output file. ~ #

	output = osmium-atmega.bin

	# ~ Specify the target clock frequency in MHz. ~ #

	clock = 16000000

	# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #

	cc = $(shell which 'avr-gcc')

	objcopy = $(shell which 'avr-objcopy')

	flasher = $(shell which 'avrdude')

	# ~ Specify platform specific preprocessor flags. ~ #

	platform_flags = -DARCH=ARCH_AVR8 -mmcu=$(platform) -D__AVR_$(shell echo $(platform) | tr a-z A-Z)__

	flasher_flags = -p $(platform) -c usbtiny -V -U lfuse:w:0xFF:m -U hfuse:w:0xD9:m -U flash:w:$(output)

	# ~ Specify dependant libraries. ~ #

	libraries = 

	# ~ Specify the platform's hardware directory. ~ #

	hardware_directory = ./hardware/atmega

else ifeq ($(platform),at91sam7s)

	# ~ Do the same for the other platforms. ~ #

	output = osmium-sam7s.bin

	clock = 48054850

	cc = $(shell which 'arm-elf-gcc-4.7')

	objcopy = $(shell which 'arm-elf-objcopy')

	flasher = $(shell which 'flipper')

	flasher_flags = flash $(output)

	platform_flags = -std=gnu99 -Os -mcpu=arm7tdmi -mtune=arm7tdmi -mthumb -mthumb-interwork -nostartfiles

	hardware_directory = ./hardware/at91sam

else

	# ~ If we have no platform, specify an error. ~ #

	$(error "Please specify a platform.")

endif

# ~ Specify platform-inspecific utilities needed to deploy for the target. ~ #

rsync = $(shell which 'rsync')

# ~ Declare the preprocessor statements to be used during object generation. ~ #

preprocess = -std=gnu99 -Os -fno-builtin-printf $(platform_flags) -DF_CPU=$(clock)UL

# ~ Pass flags to find so that it only isolates files pertainant to the target platform. ~ #

findflags = \( ! -wholename "./hardware/*" -or -wholename "$(hardware_directory)/*" -or -wholename "./hardware/common/*" \)

# ~ Specify the include directories. ~ #

include_directories = $(hardware_directory)/include ./include

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #

include rules.make

all: clean

	# ~ Link the object files together to produce an output file. ~ #

	$(cc) $(prefix) $(ldflags) $(sort $(objects)) -o $(basename $(output)).elf

	# ~ Convert the 'elf' file into a raw binary. ~ #

	$(objcopy) -O binary $(basename $(output)).elf $(output)

install:

	# ~ Create the install director if it doesn't exist. ~ #

	mkdir -p $(FLIPPERSDK)/osmium

	# ~ Copy the binary into the SDK's firmware directory. ~

	$(rsync) -r $(output) $(FLIPPERSDK)/osmium/$(output)

burn:

	# Flash the connected device with the binary using the appropriate flasher.

	$(flasher) $(flasher_flags)

clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf") $(output)