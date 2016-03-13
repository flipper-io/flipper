llvm_flags = -std=c99 -Os -target armv7-none-elf -march=arm7tdmi -mfloat-abi=soft -fPIC -nostdlib -ffreestanding -fomit-frame-pointer -fPIC

all:
	
	clang $(llvm_flags) -D__device_compilation__ -c servo.c ../common.c -I .. -I /usr/local/include -I /usr/local/include/flipper --sysroot "/Development/flipper-toolbox/resources/osx/arm-elf/arm-elf" -I "/Development/flipper-toolbox/resources/osx/arm-elf/arm-elf/include"

	arm-elf-gcc-4.7 -fPIC -nostdlib -ffreestanding -fomit-frame-pointer -Wl,-T dl.ld *.o -o servo.elf

	arm-elf-objcopy -O binary servo.elf servo.bin

	arm-elf-objdump -D servo.elf

load: all

	flipper load io.flipper.servo servo.bin

clean:

	rm -rf *.elf *.o *.bin
