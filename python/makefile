.PHONY: install

output = flipper

python_install_directory = /Library/Python/2.7/site-packages/$(output)

version = 2.7

cc = $(shell which 'gcc')

rsync = $(shell which 'rsync')

preprocess = -std=gnu99 -Os -fpic

include_directories = $(shell find '/System/Library/Frameworks/Python.framework/Versions/Current/include' -type d) $(FLIPPERSDK)/include

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #

include rules.make

# ~ Create an empty variable to synthesize the __init__.py file. ~ #

init = __all__ = [CLASSES]

all:

	# ~ Create the output directory if it doesn't exist. Do this in a shell to revoke superuser permissions. ~ #

	$(shell mkdir -p $(output))

	# ~ Compile each output file into a 'so' Python class. ~ #

	$(foreach object, $(objects), $(shell $(cc) $(prefix) -shared -lpython$(version) -lflipper $(object) -o $(output)/$(notdir $(patsubst %.o,%,$(object)).so)))

	# ~ Generate the __init__.py file. ~ #

	$(shell echo "$(subst CLASSES,$(foreach object, $(objects),'$(patsubst %.o,%,$(notdir $(object)))', ),$(init))" > $(output)/__init__.py)

install:

	# ~ Fabricate the install directory if it doesn't already exist. ~ #
	
	mkdir -p $(FLIPPERSDK)/python

	# ~ Copy the module into the SDK's python directory. ~ #

	$(rsync) -r $(output)/* $(FLIPPERSDK)/python

	# ~ Symlink the module to Python's module install directory. ~ #

	$(shell sudo ln -sfn $(FLIPPERSDK)/python $(python_install_directory))

clean:

	# ~ Remove any output files generated during compilation. ~ #

	rm -rf $(shell find . -follow -name "*.o") $(output)
