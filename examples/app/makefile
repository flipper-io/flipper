target = app
# Load into the RAM region.
region = ram

all:
	arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -g -nostartfiles -ffreestanding -fpic -Os -Wl,-T$(region).ld -I/usr/local/include main.c -o $(target).elf
	arm-none-eabi-objcopy --set-section-flags .bss=alloc,load,contents -O binary $(target).elf $(target).bin
	fdwarf $(target).elf $(target)
	gcc -I. -c $(target).c -o $(target).o
	ar -rcs $(target).a $(target).o
	rm *.o
	gcc -I. -lflipper $(target).a caller.c -o caller

dump:
	arm-none-eabi-objdump -S -z -D app.elf | less

run: all
	fload $(target).bin

clean:
	rm -rf *.o *.elf *.bin
