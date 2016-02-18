PREFIX = /usr/local

.SILENT: all install uninstall clean
.PHONY: install clean

ifndef PREFIX
$(error "Error. Please specify the environment variable 'PREFIX'. The 'PREFIX' variable will direct this install script to the install location appropriate for your system.")
endif

# ~ Remove all of the pesky '.DS_Store' files. ~ #
$(shell find . -name '.DS_Store' -exec rm -rf {} \;)

all:

	# ~ Build libflipper. ~ #
	@echo Building 'libflipper.' 📦
	$(MAKE) -C libflipper all -s

	# ~ Build the console. ~ #
	@echo Building the Flipper Console. 🖥
	$(MAKE) -C console all -s

	# ~ Build Osmium for the AVR. ~ #
	@echo Building the first stage of Osmium. 💾
	$(MAKE) -C osmium all platform=atmega16u2 -s 

	# ~ Build Osmium for the ARM. ~ #
	@echo Building the second stage of Osmium. 💾
	$(MAKE) -C osmium all platform=at91sam7s -s

	@echo The Flipper Toolbox was built successfully. 🎉

install:

	# ~ Install libflipper. ~ #
	@echo Installing 'libflipper'. 📦
	$(MAKE) -C libflipper install -s

	# ~ Install the console. ~ #
	@echo Installing the Flipper Console. 🖥
	$(MAKE) -C console install -s

	@echo The Flipper Toolbox was installed successfully. 🎉

uninstall:

	rm -rf $(PREFIX)/flipper
	rm -rf $(PREFIX)/include/flipper
	rm -rf $(PREFIX)/include/flipper.h

	@echo The Flipper Toolbox was uninstalled successfully. 💔

clean:

	# ~ Clean libflipper. ~ #
	@echo Cleaning 'libflipper'. 📦
	$(MAKE) -C libflipper clean -s

	# ~ Clean the console. ~ #
	@echo Cleaning the Flipper Console. 🖥
	$(MAKE) -C console clean -s

	# ~ Clean Osmium. ~ #
	@echo Cleaning Osmium. 💾
	$(MAKE) -C osmium clean -s

	# ~ Clean FVM. ~ #
	@echo Cleaning FVM. 💽
	$(MAKE) -C fvm clean -s

	@echo The Flipper Toolbox was cleaned successfully. 🚰
