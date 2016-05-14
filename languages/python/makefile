.PHONY: install fix

output = flipper

# ~ Determine the target. ~ #
target = $(shell echo $(shell uname -s) | tr A-Z a-z)

# ~ Set target specific macros. */
ifeq ($(target),darwin)

else ifeq ($(target),linux)

else
$(error "The operating system you are running is not supported by this build script.")
endif

cc = $(shell which 'gcc')
rsync = $(shell which 'rsync')
preprocess = -std=gnu99 -Os -fpic
include_directories = ../../include ../../libflipper/include $(shell python -c "from distutils.sysconfig import get_python_inc; print(get_python_inc())")

# ~ Include this file to recursively compile all supported file types and generate object files using the preprocessor statements above. ~ #
include rules.make

# ~ Create an empty variable to synthesize the __init__.py file. ~ #
init = __all__ = [CLASSES]

pre:

	# ~ Create the output directory if it doesn't exist. Do this in a shell to revoke superuser permissions. ~ #
	mkdir -p $(output)

all: pre $(targets)
	# ~ Generate the __init__.py file. ~ #
	echo "$(subst CLASSES,$(foreach object, $(objects),'$(patsubst %.o,%,$(notdir $(object)))', ),$(init))" > $(output)/__init__.py

$(info 'Targets: $(patsubst %.c, %.so, $(filter %.c, $(targets)))')

# ~ Python object files with the '.o' extension. ~ #
$(patsubst %.c, %.so, $(filter %.c, $(targets))) : %.so : %.c
	echo DERP
	# ~ Compile each output file into a 'so' Python class. ~ #
	#$(cc) $(prefix) -shared -lpython$(version) -lflipper $(object) -o

fix:

	$(foreach object, $(objects), $(shell $(fixup) $(output)/$(notdir $(patsubst %.o,%,$(object)).so)))

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
