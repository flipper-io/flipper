.SILENT: all install uninstall clean
.PHONY: install clean

all:

	# ~ Build osmium for the 4S. ~ #
	$(MAKE) -C targets/at91sam4s all -s

	# ~ Build osmium for the U2. ~ #
	$(MAKE) -C targets/atmega16u2 all -s

install:
	
	# ~ Install osmium for the 4S. ~ #
	$(MAKE) -C targets/at91sam4s install -s

	# ~ Install osmium for the U2. ~ #
	$(MAKE) -C targets/atmega16u2 install -s

clean:

	# ~ Clean osmium for the 4S. ~ #
	$(MAKE) -C targets/at91sam4s clean -s

	# ~ Clean osmium for the U2. ~ #
	$(MAKE) -C targets/atmega16u2 clean -s