.PHONY: all install clean

ifndef FLIPPERSDK

$(error "ERROR. The 'FLIPPERSDK' variable is not declared in your environment. Set it to specify where you would like the SDK installed.")

endif

# ~ Remove any lingering '.DS_Store' files. ~ #

$(shell find . -name '.DS_Store' -exec rm -rf {} \;)

install: all

	# ~ Build and install libflipper. ~ */

	$(MAKE) -C libflipper all install

	# ~ Build and install the console. ~ */

	$(MAKE) -C console all install

	# ~ Build and install osmium for the AVR. ~ */

	$(MAKE) -C osmium all install platform=atmega16u2

	# ~ Build and install osmium for the ARM. ~ */

	$(MAKE) -C osmium all install platform=at91sam7s

	# ~ Build and install the Python module. ~ */

	$(MAKE) -C python all fix install

uninstall:

	rm -rf /usr/local/lib/libflipper.*

	rm -rf /usr/local/include/flipper

	rm -rf /usr/local/include/flipper.h

	rm -rf $(FLIPPERSDK)

	$(MAKE) clean

clean:

	# ~ Clean libflipper. ~ */

	$(MAKE) -C libflipper clean

	# ~ Clean the console. ~ */

	$(MAKE) -C console clean

	# ~ Clean osmium. ~ */

	$(MAKE) -C osmium clean platform=atmega16u2

	# ~ Clean the FVM. ~ */

	$(MAKE) -C fvm clean

	# ~ Clean the Python module. ~ */

	$(MAKE) -C python clean
