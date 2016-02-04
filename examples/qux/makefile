all:
	$(MAKE) -C driver/device

	gcc main.c driver/common.c driver/qux.c -I driver -I $(FLIPPERSDK)/include -lflipper -o qux

load:

	flipper unload

	$(MAKE) -C driver/device load

clean:
	rm -rf qux
