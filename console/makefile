.PHONY: clean

output = flipper

find = $(shell which find)

cc = $(shell which clang)

copy = $(shell which cp)

include_dirs = ../include

includes = $(foreach dir, $(include_dirs), -I "$(dir)")

library_search_dirs = ../sdk/lib

library_dirs = $(foreach dir, $(library_search_dirs), -L "$(dir)")

libraries = flipper

libs = $(foreach lib, $(libraries), -l$(lib))

files = $(shell $(find) . -name '*.c')

flags = -std=c99 -rdynamic

prefix = $(flags) $(includes) $(library_dirs) $(libs)

all: libflipper

	$(cc) $(prefix) $(files) -o $(output)

	$(copy) $(output) ../sdk/bin

libflipper:

	$(MAKE) -C ../libflipper all install

run:

	./$(output)

clean:

	$(find) . -name '*.o' -or -name '$(output)' -exec rm -rf {} \;