target = module
# Load into the RAM region.
region = ram

all:
	# Generate device code.
	arm-none-eabi-gcc -g -nostartfiles -ffreestanding -fpic -Os -Wl,-T$(region).ld -I/usr/local/include -Iinclude -DDEVICE_BUILD device.c $(target).c -o $(target).elf
	arm-none-eabi-objcopy --set-section-flags .bss=alloc,load,contents -O binary $(target).elf $(target).bin
	# Generate host code.
	gcc -g -shared -Iinclude host.c $(target).c -lflipper -o lib$(target).dylib
	# Generate host test.
	gcc -g -Iinclude test.c -L./ -l$(target) -lflipper -o test

dump:
	arm-none-eabi-objdump -S -z -D $(target).elf | less

run: all
	fram $(target).bin

clean:
	rm -rf *.o *.elf *.bin *.dylib*
