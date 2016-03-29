.SILENT: all install uninstall clean
.PHONY: install clean

PREFIX ?= /usr/local/

# ~ Remove all of the pesky '.DS_Store' files. ~ #
$(shell find . -name '.DS_Store' -exec rm -rf {} \;)

all:

	# ~ Build libflipper. ~ #
	@echo Building 'libflipper.' 📦
	$(MAKE) -C libflipper all -s

	# ~ Build the console. ~ #
	@echo Building the Flipper Console. 🖥
	$(MAKE) -C console all -s

	# ~ Build Osmium. ~ #
	@echo Building Osmium. 💾
	$(MAKE) -C osmium all -s

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

	@echo The Flipper Toolbox was cleaned successfully. 🚰
