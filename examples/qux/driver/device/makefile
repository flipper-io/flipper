cflags = -std=c99 -Os -mcpu=arm7tdmi -nostdlib -ffreestanding -fomit-frame-pointer -fpic

flags = $(cflags) -D__device_compilation__

targets = *.c ../common.c

includes = -I .. -I . -I /usr/local/include

cc = arm-elf-gcc

objcopy = arm-elf-objcopy

all:

	# ~ Compile individual targets. ~ #
	$(cc) $(flags) $(includes) -c $(targets)

	# ~ Link objects. ~ * #
	$(cc) $(cflags) -Wl,-T dl.ld *.o -o dl.elf

	# ~ Dump loadable. ~ #
	$(objcopy) -O binary dl.elf dl.bin

dump:

	arm-elf-objdump -D dl.elf

load:

	flipper load io.flipper.dl dl.bin

launch:

	flipper launch io.flipper.dl

clean:

	rm -rf *.elf *.o *.bin
