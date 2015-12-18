llvm_flags = -target armv7-none-elf -march=arm7tdmi -mfloat-abi=soft -Os -fPIC -nostdlib -ffreestanding -Os -std=c99

all:
	
	clang $(llvm_flags) -c led.c init.s glue.c -I /usr/local/include -I /usr/local/include/flipper --sysroot "/Development/flipper-toolbox/resources/osx/arm-elf/arm-elf" -I "/Development/flipper-toolbox/resources/osx/arm-elf/arm-elf/include"  -fomit-frame-pointer

	arm-elf-gcc-4.7 -fPIC -nostdlib -ffreestanding -Wl,-T dl.ld *.o -o led.elf

	arm-elf-objcopy -O binary led.elf led.bin

	arm-elf-objdump -D led.elf

run: all

	flipper load led led.bin

clean:

	rm -rf *.elf *.o *.bin