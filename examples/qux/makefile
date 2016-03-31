all:
	$(MAKE) -C driver/device

	gcc main.c driver/common.c driver/qux.c -I driver -I /usr/local/include -lflipper -o qux

load:

	flipper unload

	$(MAKE) -C driver/device load

clean:
	$(MAKE) -C driver/device clean
	rm -rf qux
