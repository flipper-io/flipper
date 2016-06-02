cflags = -std=c99 -Os -mcpu=arm7tdmi -nostdlib -ffreestanding -fpic -D __device__
targets = *.c ../common.c

cc = arm-elf-gcc
objcopy = arm-elf-objcopy

all:

	# ~ Compile all objects. ~ * #
	$(cc) $(cflags) -I ../include -I /usr/local/include -Wl,-T $(name).ld $(targets) -o $(name).elf

	# ~ Copy the loadable ELF into binary. ~ #
	$(objcopy) -O binary $(name).elf $(name).bin

clean:

	# ~ Remove all build artifacts. ~ #
	rm -rf *.elf *.o *.bin