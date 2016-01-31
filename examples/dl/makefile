llvm_flags = -std=c99 -Os -target armv7-none-elf -march=arm7tdmi -mfloat-abi=soft -fPIC -nostdlib -ffreestanding -fomit-frame-pointer -fPIC

all:
	
	clang $(llvm_flags) -c led.c init.s glue.c -I /usr/local/include -I /usr/local/include/flipper --sysroot "/Development/flipper-toolbox/resources/osx/arm-elf/arm-elf" -I "/Development/flipper-toolbox/resources/osx/arm-elf/arm-elf/include"

	arm-elf-gcc-4.7 -fPIC -nostdlib -ffreestanding -fomit-frame-pointer -Wl,-T dl.ld *.o -o led.elf

	arm-elf-objcopy -O binary led.elf led.bin

	arm-elf-objdump -D led.elf

run: all

	flipper load io.flipper.led led.bin

clean:

	rm -rf *.elf *.o *.bin
