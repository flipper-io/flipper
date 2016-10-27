all:
	arm-none-eabi-gcc -nostdlib -ffreestanding -nostartfiles -I/usr/local/include -I include device/*.c -o device.elf
	arm-none-eabi-objcopy -O binary device.elf device.bin
	xxd -i device.bin > device.c
	gcc -shared -lflipper device.c src/*.c -I include -o qux.so
	gcc -I/usr/local/include -L/usr/local/lib -I include main.c -lflipper qux.so  -o main
install:
	mkdir -p ~/Desktop/fpg/qux
	cp include/qux/*.h ~/Desktop/fpg/qux
	cp qux.so ~/Desktop/fpg/qux
clean:
	rm -rf device.*
	rm -rf qux.*
	rm -rf main
