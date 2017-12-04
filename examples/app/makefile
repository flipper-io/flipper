target = app

all:
	arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -g -nostartfiles -ffreestanding -fPIC -Os --entry main -I/usr/local/include main.c -o main.o
	fdwarf main.o $(target)
	arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -g -nostartfiles -ffreestanding -fPIC -Os -D__DEVICE__ -I/usr/local/include -I. -c main.c -o main.o
	arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -g -nostartfiles -ffreestanding -fPIC -Os -D__DEVICE__ -I/usr/local/include -I. -c $(target).c -o $(target).o
	arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -g -nostartfiles -ffreestanding -fPIC -Os -Wl,-Tram.ld -I/usr/local/include -I. -o $(target).elf main.o $(target).o
	arm-none-eabi-objcopy --set-section-flags .bss=alloc,load,contents -O binary $(target).elf $(target).bin
	xxd -i $(target).bin > data.c
	gcc -I. -c $(target).c -o bind.o
	gcc -I. -c data.c -o data.o
	ar -rcs $(target).a bind.o data.o
	rm *.o
	gcc -I. -lflipper $(target).a caller.c -o caller

dump:
	arm-none-eabi-objdump -S -z -D app.elf | less

install: all
	fload $(target).bin

clean:
	rm -rf *.o *.elf *.bin caller data.c *.a $(target).c $(target).h
