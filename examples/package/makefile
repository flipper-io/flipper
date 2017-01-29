target = qux
# Load into the RAM region.
region = ram

# The include directories specified when building the module.
include_dirs = /usr/local/include include
# Append the -I flag to each include directory.
includes = $(foreach include,$(include_dirs),-I$(include))

# Rule to build everything.
all: device bindings host

# Rule to generate the module code for the device.
device:
	# Make the build directory if it doesn't exist.
	mkdir -p build
	# Compile the module
	arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -g -nostartfiles -ffreestanding -fpic -Os -Wl,-T$(region).ld $(includes) -DDEVICE_BUILD src/*.c $(target)/*.c -o build/$(target).elf
	# Copy the module into a binary image.
	arm-none-eabi-objcopy --set-section-flags .bss=alloc,load,contents -O binary build/$(target).elf build/$(target).bin

# Rule to generate the C binding library to the module for the host.
bindings:
	# Make the build directory if it doesn't exist.
	mkdir -p build
	# Generate host code.
	gcc -shared $(includes) src/*.c $(target)c/*.c -lflipper -install_name lib$(target)c.dylib -o build/lib$(target)c.dylib

# Rule to generate the test program for the host.
host:
	# Generate host test.
	gcc $(includes) src/*.c $(target)test/*.c -Lbuild -l$(target)c -lflipper -o build/$(target)test

# Dumps the module image for debugging purposes.
dump:
	arm-none-eabi-objdump -S -z -D $(target).elf | less

# Writes the module image to the attached device.
load: all
	fram build/$(target).bin

# Removes all build artifacts.
clean:
	rm -rf build
