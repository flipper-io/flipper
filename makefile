all: libflipper atmegau2 atsam4s

libflipper:
	mkdir -p build/library
	(cd build/library; cmake ../../library; make)

atmegau2:
	mkdir -p build/atmegau2
	(cd build/atmegau2; cmake ../../carbon/atmegau2; make)

install-atmegau2:
	dfu-programmer atmega32u2 erase --force
	dfu-programmer atmega32u2 flash build/atmegau2/atmegau2.hex
	dfu-programmer atmega32u2 start

atsam4s:
	mkdir -p build/atsam4s
	(cd build/atsam4s; cmake ../../carbon/atsam4s; make)

clean:
	rm -rf build
