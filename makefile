all:
	$(MAKE) -C products/atmegau2 all
	$(MAKE) -C products/atsam4s all
	$(MAKE) -C products/libflipper all
	$(MAKE) -C utils/fvm all

clean:
	$(MAKE) -C products/atmegau2 clean
	$(MAKE) -C products/atsam4s clean
	$(MAKE) -C products/libflipper clean
	$(MAKE) -C utils/fvm clean

install:
	$(MAKE) -C products/libflipper install
	$(MAKE) -C products/atmegau2 install
	$(MAKE) -C products/atsam4s install
