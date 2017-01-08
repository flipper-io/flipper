target = app
# Load into the RAM region.
region = ram

all:
	arm-none-eabi-gcc -g -nostartfiles -ffreestanding -fpic -Os -Wl,-T$(region).ld -I/usr/local/include $(target).c -o $(target).elf
	arm-none-eabi-objcopy --set-section-flags .bss=alloc,load,contents -O binary $(target).elf $(target).bin

run: all
	fram $(target).bin

clean:
	rm -rf *.o *.elf *.bin
