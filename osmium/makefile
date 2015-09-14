# ~ Specifiy the name of the output file. ~ #

output = osmium-atmega.bin

# ~ Specify the target platform. ~ #

target = atmega16u2

# ~ Specify the target clock frequency in MHz. ~ #

clock = 16

# ~ Specify a compiler and the other utilities needed to deploy for the target. ~ #

cc = $(shell which 'avr-gcc')

objcopy = $(shell which 'avr-objcopy')

rsync = $(shell which 'rsync')

avrdude = $(shell which 'avrdude')

# ~ Declare the preprocessor statements to be used during object generation. ~ #

preprocess = -std=gnu99 -Os -fno-builtin-printf -DARCH=ARCH_AVR8 -mmcu=$(target) -D__AVR_$(shell echo $(target) | tr a-z A-Z)__ -DF_CPU=$(clock)000000UL

# ~ Pass flags to find so that it only isolates files pertainant to the target platform. ~ #

findflags = \( ! -wholename "./hardware/*" -or -wholename "./hardware/atmega/*" \)

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #

include rules.make

all: clean

	# ~ Link the object files together to produce an output file. ~ #

	$(cc) $(prefix) $(sort $(objects)) -o $(basename $(output)).elf

	# ~ Convert the 'elf' file into a raw binary. ~ #

	$(objcopy) -O binary $(basename $(output)).elf $(output)

install:

	# ~ Create the install director if it doesn't exist. ~ #

	mkdir -p $(FLIPPERSDK)/osmium

	# Copy the binary into the SDK's firmware directory.

	$(rsync) -r $(output) $(FLIPPERSDK)/osmium/$(output)

burn:

	# Flash the connected device with the binary using 'avrdude'.

	$(avrdude) -p $(target) -c usbtiny -U lfuse:w:0xFF:m -U hfuse:w:0xD9:m -U flash:w:$(output)

clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf") $(output)