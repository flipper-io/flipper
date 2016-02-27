cflags = -std=c99 -Os -mcpu=arm7tdmi -nostdlib -ffreestanding -fomit-frame-pointer -fpic

flags = $(cflags) -D__device_compilation__

targets = *.c *.s

includes = -I .. -I .

all:

	# ~ Compile individual targets. ~ #
	arm-elf-gcc-4.7 $(flags) $(includes) -c $(targets)

	# ~ Link objects. ~ * #
	arm-elf-gcc-4.7 $(cflags) -Wl,-T dl.ld *.o -o dl.elf

	# ~ Dump loadable. ~ #
	arm-elf-objcopy -O binary dl.elf dl.bin

dump:

	arm-elf-objdump -D dl.elf

load:

	flipper load io.flipper.dl dl.bin

launch:

	flipper launch io.flipper.dl

clean:

	rm -rf *.elf *.o *.bin
