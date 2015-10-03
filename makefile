.PHONY: all install

# ~ Specify the other utilities needed by the makefile. ~ #

rsync = $(shell which 'rsync')

# ~ The 'all' target builds every component of the Flipper toolchain from source. ~ #

all:

	# ~ Build libflipper. ~ */

	$(MAKE) -C libflipper all
	
	# ~ Build the Python module. ~ */
	
	$(MAKE) -C python all

# ~ The 'install' target installs the built components of the Flipper toolchain into the Flipper SDK. ~ #

install:

	# ~ Install libflipper to the SDK. ~ */
	
	$(MAKE) -C libflipper install
	
	# ~ Install Python module to the SDK. ~ */
	
	$(MAKE) -C python install

clean:

	rm -rf $(shell find . -follow -name "*.o" -or -name "*.elf" -or -name "*.bin" -or -name "*.so" -or -name "*.dylib")
