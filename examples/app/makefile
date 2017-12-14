target = app

CFLAGS = -mcpu=cortex-m4 -g -ffreestanding -nostdlib -fPIC -Os -I/usr/local/include -I. -D__ATSAM4S__ -DPLATFORM_HEADER="<flipper/atsam4s/atsam4s.h>"

FVMFLAGS = -DPLATFORM_HEADER="<flipper/posix/posix.h>"


all:
	mkdir -p build
	arm-none-eabi-gcc $(CFLAGS) main.c -o build/main.elf
	fdwarf build/main.elf $(target) c build/cbind
	arm-none-eabi-gcc $(CFLAGS) -Wl,-Tram.ld -o build/package.elf main.c build/cbind.c
	arm-none-eabi-objcopy --set-section-flags .bss=alloc,load,contents -O binary build/package.elf package.bin
	xxd -i package.bin > build/package_data.c
	mv package.bin build
	gcc -I. -c build/cbind.c -o build/cbind_native.o
	gcc -I. -c build/package_data.c -o build/package_data_native.o
	ar -rcs build/$(target).a build/cbind_native.o build/package_data_native.o
	gcc -I. -lflipper build/$(target).a caller.c -o build/caller

fvm:
	arm-none-eabi-gcc -shared $(CFLAGS) main.c -o build/main.elf
	fdwarf build/main.elf $(target) c build/cbind
	dd if=/dev/null of=package.bin count=0
	xxd -i package.bin > build/package_data.c
	mv package.bin build
	gcc -shared -lflipper $(FVMFLAGS) main.c build/cbind.c build/package_data.c -o build/$(target).so

dump:
	arm-none-eabi-objdump -S -z -D build/package.elf | less

install: all
	fload build/package.bin

clean:
	rm -rf build
