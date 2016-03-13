all:
	$(MAKE) -C driver/device

	gcc main.c driver/common.c driver/servo.c -I driver -I $(FLIPPERSDK)/include -lflipper -o servo

load:

	flipper unload

	$(MAKE) -C driver/device load

clean:
	rm -rf servo
