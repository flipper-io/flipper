.PHONY: all install clean

# ~ The 'all' target builds every component of the Flipper toolchain from source. ~ #

all: clean

	# ~ Build libflipper. ~ */

	$(MAKE) -C libflipper all
	
	# ~ Build the console. ~ */

	$(MAKE) -C console all
	
	# ~ Build Osmium for the AVR. ~ */

	$(MAKE) -C osmium all platform=atmega16u2
	
	# ~ Build Osmium for the ARM. ~ */

	$(MAKE) -C osmium all platform=at91sam7s
	
	# ~ Build the Python module. ~ */
	
	$(MAKE) -C python all

# ~ The 'install' target installs the built components of the Flipper toolchain into the Flipper SDK. ~ #

install:

	# ~ Install libflipper. ~ */

	$(MAKE) -C libflipper install
	
	# ~ Install the console. ~ */

	$(MAKE) -C console install
	
	# ~ Install Osmium for the AVR. ~ */

	$(MAKE) -C osmium install platform=atmega16u2
	
	# ~ Install Osmium for the ARM. ~ */

	$(MAKE) -C osmium install platform=at91sam7s
	
	# ~ Install the Python module. ~ */
	
	$(MAKE) -C python install

clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf" -or -name "*.bin" -or -name "*.so" -or -name "*.dylib")
