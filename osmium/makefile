.SILENT: all install uninstall clean
.PHONY: all install uninstall clean

all:

	# ~ Build Osmium for the 4S. ~ #
	$(MAKE) -C targets/at91sam4s all -s

	# ~ Build Osmium for the U2. ~ #
	$(MAKE) -C targets/atmega16u2 all -s

install:

	# ~ Install Osmium for the 4S. ~ #
	$(MAKE) -C targets/at91sam4s install -s

	# ~ Install Osmium for the U2. ~ #
	$(MAKE) -C targets/atmega16u2 install -s

clean:

	# ~ Clean Osmium for the 4S. ~ #
	$(MAKE) -C targets/at91sam4s clean -s

	# ~ Clean Osmium for the U2. ~ #
	$(MAKE) -C targets/atmega16u2 clean -s
