# ~ Determine the target. ~ #

target = $(shell echo $(shell uname -s) | tr A-Z a-z)

# ~ Shortcuts for the platform names. ~ #

ifeq ($(p),u2)

platform = atmega16u2

else ifeq ($(p),7s)

platform = at91sam7s

endif

# ~ Determine which platform is being targeted and define its build macros. ~ */

ifeq ($(platform),atmega16u2)

# ~ Specifiy the name of the output file. ~ #

output = osmium-atmega.bin

# ~ Specify the target clock frequency in MHz. ~ #

clock = 16000000

# ~ Specify a compiler and the binary utilities needed to deploy code to the target. ~ #

cc = $(shell which 'avr-gcc')

objcopy = $(shell which 'avr-objcopy')

uploader = $(shell which 'avrdude')

# ~ Specify platform specific preprocessor flags. ~ #

uploader_flags = -p $(platform) -c usbtiny -V -U lfuse:w:0xFF:m -U hfuse:w:0xD9:m -U flash:w:$(output)

platform_flags = -DARCH=ARCH_AVR8 -mmcu=$(platform) -D__AVR_$(shell echo $(platform) | tr a-z A-Z)__ -D__atmega_build__

# ~ Specify the platform's hardware directory. ~ #

hardware_directory = ./hardware/atmega

else ifeq ($(platform),at91sam7s)

# ~ Specifiy the name of the output file. ~ #

output = osmium-sam7s.bin

# ~ Specify the target clock frequency in MHz. ~ #

clock = 48054850

# ~ Specify a compiler and the binary utilities needed to deploy code to the target. ~ #

ifeq ($(target),darwin)

cc = /opt/local/bin/arm-elf-gcc-4.7

objcopy = /opt/local/bin/arm-elf-objcopy

else ifeq ($(target),linux)

cc = ../resources/linux/arm-elf/bin/arm-elf-gcc

objcopy = ../resources/linux/arm-elf/bin/arm-elf-objcopy

else

# ~ If we have no target, specify an error. ~ #

$(error "Please specify a target.")

endif

uploader = $(shell which 'flipper')

# ~ Specify platform specific preprocessor flags. ~ #

uploader_flags = flash $(output)

platform_flags = -mcpu=arm7tdmi -mtune=arm7tdmi -mthumb -mthumb-interwork -nostartfiles -ffreestanding -fdata-sections -ffunction-sections -fomit-frame-pointer

# ~ Specify the platform's hardware directory. ~ #

hardware_directory = ./hardware/at91sam

else

# ~ If we haven't been given a target platform, throw an error. ~ #

$(error "ERROR. No target platform has been specified. See BUILD for more information.")

endif

# ~ Specify platform-inspecific utilities needed to deploy for the target. ~ #

rsync = $(shell which 'rsync')

# ~ Declare the preprocessor statements to be used during object generation. ~ #

preprocess = -std=gnu99 -Os -fno-builtin-printf $(platform_flags) -DF_CPU=$(clock)UL

# ~ Pass flags to find so that it only isolates files pertainant to the target platform. ~ #

findflags = \( ! -wholename "./hardware/*" -or -wholename "$(hardware_directory)/*" -or -wholename "./hardware/common/*" \)

# ~ Specify the include directories. ~ #

include_directories = $(hardware_directory)/include ./include ./hardware/at91sam/modules

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

	# ~ Copy the binary into the SDK's firmware directory. ~ #

	$(rsync) -r $(output) $(FLIPPERSDK)/osmium/$(output)

burn:

	# ~ Flash the connected device with the binary using the appropriate uploader. ~ #

	$(uploader) $(uploader_flags)

clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf") *.bin
