output = qux_test
module = qux

cc = gcc

all:

	# ~ Build the module. ~ #
	$(MAKE) -C $(module)

	# ~ Compile the interface application. ~ #
	$(cc) *.c -I $(module)/include $(module)/$(module).a -lflipper -o $(output)

clean:
	$(MAKE) -C $(module) clean
	rm -rf *.o $(output)
