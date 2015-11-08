.PHONY: all install clean

ifndef FLIPPERSDK

$(error "ERROR. The 'FLIPPERSDK' variable is not declared in your environment. Set it to specify where you would like the SDK installed.")

endif

# ~ The 'all' target builds every component of the Flipper toolchain from source. ~ #

all: clean

	# ~ Build libflipper. ~ */

	$(MAKE) -C libflipper all

	# ~ Build the Console. ~ */

	$(MAKE) -C console all

	# ~ Build Osmium for the AVR. ~ */

	$(MAKE) -C osmium all platform=atmega16u2

	# ~ Build Osmium for the ARM. ~ */

	$(MAKE) -C osmium all platform=at91sam7s

	# ~ Build the Python module. ~ */

	$(MAKE) -C python all

# ~ The 'install' target installs the built components of the Flipper toolchain into the Flipper SDK. ~ #

install: all

	# ~ Install libflipper. ~ */

	$(MAKE) -C libflipper install

	# ~ Install the Console. ~ */

	$(MAKE) -C console install

	# ~ Install Osmium for the AVR. ~ */

	$(MAKE) -C osmium install platform=atmega16u2

	# ~ Install Osmium for the ARM. ~ */

	$(MAKE) -C osmium install platform=at91sam7s

	# ~ Install the Python module. ~ */

	$(MAKE) -C python install

uninstall:

	rm -rf /usr/local/lib/libflipper.*

	rm -rf /usr/local/include/flipper

	rm -rf /usr/local/include/flipper.h

	rm -rf $(FLIPPERSDK)

	$(MAKE) clean

clean:

	# ~ Clean libflipper. ~ */

	$(MAKE) -C libflipper clean

	# ~ Clean the Console. ~ */

	$(MAKE) -C console clean

	# ~ Clean Osmium. ~ */

	$(MAKE) -C osmium clean platform=atmega16u2

	# ~ Clean the FVM. ~ */

	$(MAKE) -C fvm clean

	# ~ Clean the Python module. ~ */

	$(MAKE) -C python clean
