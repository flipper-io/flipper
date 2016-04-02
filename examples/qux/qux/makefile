name = qux
output = $(name).module

cc = $(shell which 'gcc')
ar = $(shell which 'ar')
zip = $(shell which 'zip')

all:

	# ~ Build the module for the host. ~ #
	$(cc) -I include -c *.c
	$(ar) rcs $(name).a *.o

	# ~ Build the module for the device. ~ #
	$(MAKE) -C device name=$(name)
	mv device/$(name).bin .

	# ~ Create the module archive. ~ #
	$(zip) -r -X $(output) "$(name).a" "include" "$(name).bin"

clean:

	# ~ Remove build artifacts. ~ #
	$(MAKE) -C device clean
	rm -rf *.a *.o *.bin *.module
