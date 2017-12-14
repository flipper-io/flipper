target = app

CFLAGS = -mcpu=cortex-m4 -mthumb -g -nostartfiles -ffreestanding -fPIC -Os -I/usr/local/include -I.

DFLAGS = -D__DEVICE__ -DPLATFORM_HEADER="<flipper/atsam4s/atsam4s.h>" 

all:
	mkdir -p build
	arm-none-eabi-gcc $(CFLAGS) $(DFLAGS) main.c -o build/main.o
	fdwarf build/main.o $(target) c build/cbind
	arm-none-eabi-gcc $(CFLAGS) $(DFLAGS) -c main.c -o build/main.o
	arm-none-eabi-gcc $(CFLAGS) $(DFLAGS) -c build/cbind.c -o build/cbind.o
	arm-none-eabi-gcc $(CFLAGS) $(DFLAGS) -o build/package.elf build/main.o build/cbind.o
	arm-none-eabi-objcopy --set-section-flags .bss=alloc,load,contents -O binary build/package.elf package.bin
	xxd -i package.bin > build/package_data.c
	mv package.bin build
	gcc -I. -c build/cbind.c -o build/cbind_native.o
	gcc -I. -c build/package_data.c -o build/package_data_native.o
	ar -rcs build/$(target).a build/cbind_native.o build/package_data_native.o
	gcc -I. -lflipper build/$(target).a caller.c -o build/caller

dump:
	arm-none-eabi-objdump -S -z -D build/package.elf | less

install: all
	fload build/package.bin

clean:
	rm -rf build
